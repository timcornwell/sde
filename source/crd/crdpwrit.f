C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdpwrit.f	1.1 12/23/92
C
      SUBROUTINE CRDPWRIT (NAME)
C
CD Write coordinate system to PVM
C
C
C	NAME	CH*(*)	input	Name of directory entry
C Audit trail:
C
C	Original version.
C
C	Update pvm2.4 -> pvm3.3.
C				J. Pedelty	Feb 15 1995
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"fpvm3.h"
C
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDPWRIT')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), 
     1   		DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
C
      INTEGER		IAX, INFO
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     1   ROTA)
C
      CALL PVMFPACK (INTEGER4, NAX, 1, 1, INFO)
      CALL PVMFPACK (INTEGER4, NAXIS, NAX, 1, INFO)
      DO 10 IAX = 1, NAX
         CALL PVMFPACK (STRING, TYPE(IAX), 8, 1, INFO)
 10   CONTINUE
      CALL PVMFPACK (REAL4, RPIX, NAX, 1, INFO)
      CALL PVMFPACK (REAL4, DELT, NAX, 1, INFO)
      CALL PVMFPACK (REAL4, ROTA, NAX, 1, INFO)
      CALL PVMFPACK (REAL8, RVAL, NAX, 1, INFO)
      IF(INFO.LT.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM send error')
      ENDIF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
