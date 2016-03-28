C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrfftrh.f	1.1 12/5/92
C
      SUBROUTINE ARRFFTRH (NPAR, RARRAY, XARRAY, DIR)
C
CD HOST for PVM implementation of ARRFFTR
C
C ARRFFTRH is the PVM host for a number of instances of ARRFFTR.
C A collection of NPAR arrays are passed down to this routine.
C It then creates a PVM server, ARRFFTRS, for each instance and
C send each one the appropriate data. When all transmissions have
C finished, it then waits for answers. As each answer arrives,
C it is put into the relevant place in the RARRAY and XARRAY
C arrays.
C
C	NPAR	INT	input	Number of parallel instances
C	RARRAY(*)	CH*(*)	input	Names of real arrays
C	XARRAY(*)	CH*(*)	input	Names of complex arrays
C	DIR	CH*(*)	input	Direction of transform FPS convention
C
C Audit trail:
C
C       Original version.
C
C       Update from pvm2.4 -> pvm3.3.
C                                       J. Pedelty      Feb 15, 1995
C       Remove unused "CHARACTER*(*)     PVMHOSTNAME, ARCH" line
C	(unused variables); sgi/R8000 won't allow assumed-length char
C	strings anyway.
C                                       M. Stupar	Mar 13, 1995
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"fpvm3.h"
C
      CHARACTER*(*)	RARRAY(*), XARRAY(*)
      INTEGER		DIR, NPAR
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRFFTRH')
C
      INTEGER		MSGTYPE, INFO, I, INST, HOSTID, ID
      INTEGER           NUMTASK, NUMTOSPAWN, BUFID, SPEED
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL PVMFMYTID ( HOSTID )
      IF(HOSTID.LT.0) THEN
         CALL ERRREPOR (ERRLOGIC, 'PVM failure in pvmfmytid as host',
     $      ROUTINE)
         GO TO 999
      ENDIF
C
C Now set up the NPAR instances we need and send data
C
      MSGTYPE=1
      NUMTOSPAWN=1
      DO 5 I = 1, NPAR
         CALL PVMFSPAWN ( 'arrfftrs\0', PVMTASKARCH, '*',
     $                    NUMTOSPAWN, ID, NUMTASK)
         IF(NUMTASK.LT.0) THEN
            CALL ERRREPOR (ERRLOGIC,
     $         'PVM system failure in pvmfspawn', ROUTINE)
            GO TO 999
         ENDIF
         IF(NUMTASK.LT.NUMTOSPAWN) THEN
            CALL ERRREPOR (ERRLOGIC,
     $         'Task spawning failure in pvmfspawn', ROUTINE)
            GO TO 999
         ENDIF
         CALL PVMFINITSEND ( PVMDATARAW, INFO )
         CALL PVMFPACK (INTEGER4, I, 1, 1, INFO)
         CALL PVMFPACK (INTEGER4, DIR, 1, 1, INFO)
         CALL DATPWRIT (RARRAY)
         CALL DATPWRIT (XARRAY)
         CALL PVMFSEND (ID, MSGTYPE, INFO)
         IF(INFO.LT.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM send error')
            GO TO 990
         ENDIF
 5    CONTINUE
C
      MSGTYPE=2
      DO 20 INST = 1, NPAR
         CALL PVMFRECV (-1, MSGTYPE, BUFID)
         IF(BUFID.LT.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM receive error')
            GO TO 21
         ENDIF
         CALL PVMFUNPACK (INTEGER4, I, 1, 1, INFO)
         CALL PVMFUNPACK (INTEGER4, DIR, 1, 1, INFO)
         CALL DATDELET (RARRAY(I))
         CALL DATCREAT (RARRAY(I))
         CALL DATPREAD (RARRAY(I))
         CALL DATDELET (XARRAY(I))
         CALL DATCREAT (XARRAY(I))
         CALL DATPREAD (XARRAY(I))
         IF(ERROR) GOTO 21
 20   CONTINUE
 21   CONTINUE
C
      CALL PVMFEXIT (INFO)
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
