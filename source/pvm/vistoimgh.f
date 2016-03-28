C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vistoimgh.f	1.3 12/24/92
C
      SUBROUTINE VISTOIMGH (NPAR, VIS, SUB, IMG, GRID, PSF)
C
CD Parallel interface to VISTOIMG
C
C VISTOIMGH provides a parallel interface to VISTOIMG. A number
C of images can be made from the same data set. If PVM is available
C and the parallel system is enabled then NPROC servers are
C instantiated (NPROC is the number of processors available).
C Each server is primed with visibility data and then fed image
C templates. As each image is calculated, it is sent back and stored
C in the appropriate location. Every time a server returns with a 
C result, it is given a new template until all the work is done.
C If PVM is not available or enabled then the calculation is done
C serially.
C
C	NPAR	INT	input	Number of images
C	VIS	CH*(*)	input	Name of visibility set
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C	IMG	CH*(*)	input	Name of output images
C	GRID	CH*(*)	input	Name of grids for gridding
C	PSF	LOG	input	TRUE for PSF calculation
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Dec 1 1992
C	Update from pvm2.4 -> pvm3.3.
C				J. Pedelty	Feb 15 1995
C----------------------------------------------------------------------
#include	"stdinc.h"
#include	"fpvm3.h"
C
C Arguments go here
C
      INTEGER		NPAR
      CHARACTER*(*)	VIS, SUB, IMG(*), GRID(*)
      LOGICAL		PSF(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISTOIMGH')
C
      INTEGER		I
#if PVM
C
      CHARACTER*(SYSMXNAM)	STRM2, STRM3, SERVER
C
      INTEGER		ID(1000), SPEED(1000), PVMDTID(1000)
      INTEGER           INFO, MSGTYPE, HOSTID, J, SID, LEN
      INTEGER           NUMTOSPAWN, NUMT, BUFID
      INTEGER		NPROC, IPROC, IDONE, NFORMAT, IPAR
      CHARACTER 	PVMHOSTNAME(1000)*16, ARCH(1000)*8
C=======================================================================
      IF (ERROR) GO TO 999
C
C Is the parallel system enabled?
C
      IF(.NOT.SYSPAR) THEN
         GO TO 800
      ENDIF
C
C Enroll with PVM
C
      CALL PVMFMYTID (HOSTID)
      IF(HOSTID.LT.0) THEN
         CALL MSGPUT ('PVM failure in pvmfmytid as host, do serially',
     $      'I')
         GO TO 800
      ELSE
         WRITE (MESSAGE,1000) HOSTID
 1000    FORMAT ('Enrolled with PVM: host id = ', Z8)
         CALL MSGPUT (MESSAGE, 'I')
      ENDIF
C
C This will allow XPVM to trace the process.
C
      CALL SDE_INITXPVM ()
C
C Uncomment with caution!  Intended to catch slave output, but can
C   cause program termination!
C
C     CALL PVMFCATCHOUT (1, INFO)
C
C Find the number of processors available. We only start one
C server per processor.
C
      CALL PVMFCONFIG (NPROC, NFORMAT, PVMDTID(1), PVMHOSTNAME(1),
     $                 ARCH(1), SPEED(1), INFO)
      DO 2 I = 2, NPROC
         CALL PVMFCONFIG (NPROC, NFORMAT, PVMDTID(I), PVMHOSTNAME(I),
     $                    ARCH(I), SPEED(I), INFO)
 2    CONTINUE
C
      DO 3 I = 1, NPROC
         WRITE (MESSAGE,1010) I, PVMDTID(I), PVMHOSTNAME(I), ARCH(I)
 1010    FORMAT ('PVM vistoimgh ',I3,
     .           ': DTID= ', Z8,
     .           ': host= ', A16,
     .           ': arch= ', A8)
         CALL MSGPUT (MESSAGE, 'I')
 3    CONTINUE
C
      NPROC = MIN(NPAR, NPROC)
C
C Now set up the NPROC server instances we need
C
      NUMTOSPAWN=1
      DO 5 IPROC = 1, NPROC
         CALL PVMFSPAWN ( 'vistoimgs', PVMDEFAULT, '*',
     $                    NUMTOSPAWN, ID(IPROC), NUMT)
         IF(NUMT.LE.0) THEN
            CALL MSGPUT ('PVM failure to initiate, do serially', 'I')
            DO 15 J = 1, IPROC-1
               CALL PVMFKILL ( ID(J), INFO )
 15         CONTINUE
            CALL PVMFEXIT (INFO)
            GO TO 800
         ENDIF
 5    CONTINUE
C
C Send visibility data globally to all servers as message 1
C
      CALL PVMFINITSEND( PVMDATARAW, BUFID)
      CALL CRDPWRIT (STRM2(VIS,SUB))
      CALL ARRPWRIT (STRM3(VIS,SUB,'VIS'))
      CALL ARRPWRIT (STRM3(VIS,SUB,'WT'))
      CALL ARRPWRIT (STRM2(VIS,'UU'))
      CALL ARRPWRIT (STRM2(VIS,'VV'))
      CALL ARRPWRIT (STRM2(VIS,'WW'))
      CALL PVMFMCAST ( NPROC, ID, 1, INFO)
      IF(INFO.LT.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM send error')
         GO TO 990
      ENDIF
C
C Prime NPROC servers to do the work by sending each an initial
C image template. We also send the number of each image so that
C when returned we know where to put it.
C
      IPAR=0
      IDONE=0
      DO 8 IPROC = 1, NPROC
         IPAR=IPAR+1
         CALL PVMFINITSEND ( PVMDATARAW, BUFID )
         CALL PVMFPACK (INTEGER4, IPAR, 1, 1, INFO)
         IF(PSF(IPAR)) THEN
            CALL PVMFPACK (INTEGER4, 1, 1, 1, INFO)
         ELSE
            CALL PVMFPACK (INTEGER4, 0, 1, 1, INFO)
         END IF
         CALL DATPWRIT (IMG(IPAR))
         IF(ERROR) GO TO 990
         CALL PVMFSEND ( ID(IPROC), 2, INFO )
         IF(INFO.LT.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM send error')
            GO TO 990
         ENDIF
 8    CONTINUE
C
C Now start loop: First we look for a result. If ok then we 
C get it and start another using the server which just reported.
C Message 11 is ok, message 999 means an error has occured in the
C server.
C
 20   CONTINUE
      CALL PVMFRECV (-1, -1, BUFID)
      CALL PVMFBUFINFO (BUFID, LEN, MSGTYPE, SID, INFO) 
      IF(MSGTYPE.EQ.11) THEN
         CALL PVMFUNPACK (INTEGER4, I, 1, 1, INFO)
         CALL ARRPREAD (IMG(I))
         IDONE=IDONE+1
         IF(IPAR.LT.NPAR) THEN
            IPAR=IPAR+1
            CALL PVMFINITSEND ( PVMDATARAW, BUFID)
            CALL PVMFPACK (INTEGER4, IPAR, 1, 1, INFO)
            IF(PSF(IPAR)) THEN
               CALL PVMFPACK (INTEGER4, 1, 1, 1, INFO)
            ELSE
               CALL PVMFPACK (INTEGER4, 0, 1, 1, INFO)
            ENDIF
            CALL DATPWRIT (IMG(IPAR))
            IF(ERROR) GO TO 990
            CALL PVMFSEND (SID, 2, INFO)
            IF(INFO.LT.0) THEN
               CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM send error')
               GO TO 990
            ENDIF
         ENDIF
      ELSEIF(MSGTYPE.EQ.999) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'vistoimgs failed')
         GO TO 990
      END IF
      IF(ERROR) GOTO 990
      IF(IDONE.LT.NPAR) GOTO 20
C
C Trace any SDE errors
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
C Kill all servers
C
      DO 30 IPROC = 1, NPROC
         CALL PVMFKILL (ID(IPROC), INFO)
 30   CONTINUE
C
C Un-enroll from PVM
C
      CALL PVMFEXIT (INFO)
      GO TO 999
C
C Alternate path: Do in series
C
 800  CONTINUE
      DO 10 I = 1, NPAR
         CALL VISTOIMG (VIS, SUB, IMG(I), GRID(I), PSF(I))
 10   CONTINUE
C
C
  999 CONTINUE
      END
#else
C=======================================================================
      IF (ERROR) GO TO 999
C
C No PVM (alas!): do in series
C
      DO 10 I = 1, NPAR
         CALL VISTOIMG (VIS, SUB, IMG(I), GRID(I), PSF(I))
 10   CONTINUE
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
#endif
