C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)flytovish.f	1.4 12/29/92
C
      SUBROUTINE FLYTOVISH (NPAR, VIS, SUB, TSUB, IMG, WRK, GRID,
     $   FSWITCH)
C
CD Parallel interface to FLYTOVISH
C
C Audit trail:
C
C	Original version.
C
C	Update from pvm2.4 -> pvm3.3.
C					J. Pedelty	Feb 15, 1995
C----------------------------------------------------------------------
#include	"stdinc.h"
#include	"fpvm3.h"
C
C Arguments go here
C
      INTEGER		NPAR
      CHARACTER*(*)	VIS, SUB, TSUB, IMG(*), WRK(*), GRID(*)
      REAL		FSWITCH
C
C End of arguments
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYTOVISH')
C
      INTEGER		I
#if PVM
      CHARACTER*(SYSMXNAM)	STRM2, STRM3, SERVER
C
      INTEGER		INFO, MSGTYPE, HOSTID, J, SID, LEN
      INTEGER		NPROC, IPROC, IDONE, NFORMAT, IPAR
      INTEGER           NUMT, NUMTOSPAWN, BUFID
      INTEGER           ID(1000), SPEED(1000), PVMDTID(1000)
      CHARACTER 	PVMHOSTNAME(1000)*24, ARCH(1000)*8
C=======================================================================
      IF (ERROR) GO TO 999
C
      IF(.NOT.SYSPAR) THEN
         GO TO 800
      ENDIF
C
      CALL PVMFMYTID (HOSTID)
      IF(HOSTID.LT.0) THEN
         CALL MSGPUT ('PVM failure in pvmftid as host, do serially',
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
      CALL PVMFCONFIG (NPROC, NFORMAT, PVMDTID(1), PVMHOSTNAME(1),
     $                 ARCH(1), SPEED(1), INFO)
      DO 2 I = 2, NPROC
         CALL PVMFCONFIG (NPROC, NFORMAT, PVMDTID(I), PVMHOSTNAME(I),
     $                    ARCH(I), SPEED(I), INFO)
 2       CONTINUE
C
      DO 3 I = 1, NPROC
         WRITE (MESSAGE,1010) I, PVMDTID(I), PVMHOSTNAME(I), ARCH(I)
 1010    FORMAT ('PVM flytovis ',I3,
     .           ': DTID= ', Z8,
     .           ': host= ', A16,
     .           ': arch= ', A8)
         CALL MSGPUT (MESSAGE, 'I')
 3    CONTINUE
C
      NPROC = MIN(NPAR, NPROC)
C
C Now set up the NPROC instances we need
C
      NUMTOSPAWN=1
      DO 5 IPROC = 1, NPROC
         CALL PVMFSPAWN ('flytoviss', PVMDEFAULT, '*',
     $                   NUMTOSPAWN, ID(IPROC), NUMT)
         IF(NUMT.LE.0) THEN
            CALL MSGPUT ('PVM failure to initiate, do serially', 'I')
            DO 15 J = 1, IPROC-1
               CALL PVMFKILL (ID(J), INFO)
 15         CONTINUE
            CALL PVMFEXIT(INFO)
            GO TO 800
         ENDIF
 5    CONTINUE
C
C Send visibility data globally
C
      CALL PVMFINITSEND(PVMDATARAW, BUFID)
      CALL CRDPWRIT (STRM2(VIS,TSUB))
      CALL ARRPWRIT (STRM3(VIS,TSUB,'VIS'))
      CALL ARRPWRIT (STRM3(VIS,TSUB,'WT'))
      CALL ARRPWRIT (STRM2(VIS,'UU'))
      CALL ARRPWRIT (STRM2(VIS,'VV'))
      CALL ARRPWRIT (STRM2(VIS,'WW'))
      CALL PVMFPACK (REAL4, FSWITCH, 1, 1, INFO)
      CALL PVMFMCAST (NPROC, ID, 1, INFO)
      IF(INFO.LT.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM send error')
         GO TO 990
      ENDIF
C
C Prime NPROC servers to do the work.  Send templates as message 2.
C
      IPAR=0
      IDONE=0
      DO 8 IPROC = 1, NPROC
         IPAR=IPAR+1
         CALL PVMFINITSEND (PVMDATARAW, BUFID)
         CALL PVMFPACK (INTEGER4, IPROC, 1, 1, INFO)
         CALL DATPWRIT (IMG(IPAR))
         CALL PVMFSEND (ID(IPROC), 2, INFO)
         IF(INFO.LT.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM send error')
            GO TO 990
         ENDIF
         IF(ERROR) GO TO 990
 8    CONTINUE
C
C Now start loop: First we look for a result. If ok then we 
C get it and start another using the server which just reported
C
 20   CONTINUE
      CALL PVMFRECV (-1, -1, BUFID)
      CALL PVMFBUFINFO (BUFID, LEN, MSGTYPE, SID, INFO) 
      IF(MSGTYPE.EQ.11) THEN
         CALL PVMFUNPACK (INTEGER4, IPROC, 1, 1, INFO)
         IDONE=IDONE+1
         IF(IPAR.LT.NPAR) THEN
            IPAR=IPAR+1
            CALL PVMFINITSEND (PVMDATARAW, INFO)
            CALL PVMFPACK (INTEGER4, IPROC, 1, 1, INFO)
            CALL DATPWRIT (IMG(IPAR))
            CALL PVMFSEND (SID, 2, INFO)
            IF(INFO.LT.0) THEN
               CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM send error')
               GO TO 990
            ENDIF
         ENDIF
      ELSEIF(MSGTYPE.EQ.12) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Inappropriate PVM message 12')
         GO TO 990
      ELSEIF(MSGTYPE.EQ.999) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'flytoviss failed')
         GO TO 990
      END IF
      IF(ERROR) GOTO 990
      IF(IDONE.LT.NPAR) GOTO 20
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
C Now tell all the servers to report back
C
      DO 30 IPROC = 1, NPROC
         CALL PVMFINITSEND(PVMDATARAW, INFO)
         CALL PVMFSEND (ID(IPROC), 3, INFO)
 30   CONTINUE
C
C Ask for the results
C
      DO 40 IPROC = 1, NPROC
         CALL PVMFRECV (-1, -1, BUFID)
         CALL PVMFBUFINFO (BUFID, LEN, MSGTYPE, SID, INFO)
         IF(MSGTYPE.EQ.12) THEN
            CALL ARRPREAD (STRM3(VIS,TSUB,'VIS'))
            CALL ARRADD (STRM3(VIS,SUB,'VIS'),STRM3(VIS,TSUB,'VIS'),
     $         STRM3(VIS,SUB,'VIS'))
         ELSEIF(MSGTYPE.EQ.11) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $         'Inappropriate PVM message 11')
         ELSEIF(MSGTYPE.EQ.999) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'flytoviss failed')
         END IF
 40   CONTINUE
C
      CALL PVMFEXIT(INFO)
      GO TO 999
C
C Do in series
C
 800  CONTINUE
      DO 10 I = 1, NPAR
         CALL FLYTOVIS (VIS, SUB, TSUB, IMG(I), WRK(I), GRID(I),
     $      FSWITCH)
 10   CONTINUE
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
         CALL FLYTOVIS (VIS, SUB, TSUB, IMG(I), WRK(I), GRID(I),
     $      FSWITCH)
 10   CONTINUE
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
#endif
