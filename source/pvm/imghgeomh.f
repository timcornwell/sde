C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imghgeomh.f	1.1 12/29/92
C
      SUBROUTINE IMGHGEOMH (NPAR, IMG, TEMPLATE, OUT, INTERP)
C
CD Parallel interface to IMGHGEOM
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
      CHARACTER*(*)	IMG(*), TEMPLATE, OUT, INTERP
C
C End of arguments
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGHGEOMH')
C
      INTEGER		I
#if PVM
C
      INTEGER		ID(1000), INFO, MSGTYPE, HOSTID, J, SID, LEN
      INTEGER		MSGTYPES(3), NPROC, IPROC, IDONE, NFORMAT, IPAR
      INTEGER           NUMT, NUMTOSPAWN, BUFID, SPEED, PVMDTID
      DATA		MSGTYPES	/11,12,999/
      CHARACTER 	PVMHOSTNAME*24, ARCH*8
C=======================================================================
      IF (ERROR) GO TO 999
C
      IF(.NOT.SYSPAR) THEN
         GO TO 800
      ENDIF
C
      CALL PVMFMYTID (HOSTID)
      IF(HOSTID.LT.0) THEN
         CALL MSGPUT ('PVM failure in pvmfmytid as host, do serially',
     $      'I')
         GO TO 800
      ENDIF
C
      CALL PVMFCONFIG (NPROC, NFORMAT, PVMDTID, PVMHOSTNAME,
     $                 ARCH, SPEED, INFO)
      NPROC = MIN(NPAR, NPROC)
C
C Now set up the NPROC instances we need
C
      NUMTOSPAWN=1
      DO 5 IPROC = 1, NPROC
         CALL PVMFSPAWN ('imghgeoms\0', PVMTASKARCH, '*', NUMTOSPAWN,
     $                   ID(IPROC), NUMT)
         IF(ID(IPROC).LT.0) THEN
            CALL MSGPUT ('PVM failure to initiate, do serially', 'I')
            DO 15 J = 1, I-1
               CALL PVMFKILL (ID(J), INFO)
 15         CONTINUE
            CALL PVMFEXIT (INFO)
            GO TO 800
         ENDIF
 5    CONTINUE
C
C Send Template globally
C
      CALL PVMFINITSEND(PVMDATARAW, INFO)
      CALL DATPWRIT (TEMPLATE)
      CALL PVMFPACK (STRING, INTERP, SYSMXNAM, 1, INFO)
      CALL PVMFMCAST (NPROC, ID, 1, INFO)
      IF(INFO.LT.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM send error')
         GO TO 990
      ENDIF
C
C Prime NPROC servers to do the work
C
      IPAR=0
      IDONE=0
      DO 8 IPROC = 1, NPROC
         IPAR=IPAR+1
         CALL PVMFINITSEND (PVMDATARAW, INFO)
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
      CALL PVMFRECV (-1, 3, BUFID)
      CALL PVMFBUFINFO (BUFID, LEN, MSGTYPE, HOSTID, INFO)
      IF(MSGTYPE.EQ.11) THEN
         CALL PVMFUNPACK (INTEGER4, IPROC, 1, 1, INFO)
         IDONE=IDONE+1
         IF(IPAR.LT.NPAR) THEN
            IPAR=IPAR+1 
            CALL PVMFINITSEND (PVMDATARAW, INFO)
            CALL PVMFPACK (INTEGER4, IPROC, 1, 1, INFO)
            CALL DATPWRIT (IMG(IPAR))
            CALL PVMFSEND (ID(IPROC), 2, INFO)
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
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'imghgeoms failed')
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
      CALL IMGCLONE (TEMPLATE, ROUTINE)
      DO 40 IPROC = 1, NPROC
         CALL PVMFRECV (-1, 3, BUFID)
         CALL PVMFBUFINFO (BUFID, LEN, MSGTYPE, HOSTID, INFO)
         IF(MSGTYPE.EQ.12) THEN
            CALL ARRPREAD (ROUTINE)
            CALL ARRADD (OUT, ROUTINE, OUT)
         ELSEIF(MSGTYPE.EQ.11) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $         'Inappropriate PVM message 11')
         ELSEIF(MSGTYPE.EQ.999) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'imghgeoms failed')
         END IF
 40   CONTINUE
      CALL DATDELET (ROUTINE)
C
      CALL PVMFEXIT(INFO)
      GO TO 999
C
C Do in series
C
 800  CONTINUE
      DO 10 I = 1, NPAR
         CALL IMGHGEOM (IMG(I), TEMPLATE, OUT, INTERP)
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
         CALL IMGHGEOM (IMG(I), TEMPLATE, OUT, INTERP)
 10   CONTINUE
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
#endif
