C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdpread.f	1.1 12/23/92
C
      SUBROUTINE CRDPREAD (NAME)
C
CD Read coordinate system from PVM
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
      PARAMETER		(ROUTINE = 'CRDPREAD')
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
      CALL PVMFUNPACK (INTEGER4, NAX, 1, 1, INFO)
      CALL PVMFUNPACK (INTEGER4, NAXIS, NAX, 1, INFO)
      DO 10 IAX = 1, NAX
         CALL PVMFUNPACK (STRING, TYPE(IAX), 8, 1, INFO)
 10   CONTINUE
      CALL PVMFUNPACK (REAL4, RPIX, NAX, 1, INFO)
      CALL PVMFUNPACK (REAL4, DELT, NAX, 1, INFO)
      CALL PVMFUNPACK (REAL4, ROTA, NAX, 1, INFO)
      CALL PVMFUNPACK (REAL8, RVAL, NAX, 1, INFO)
      IF(INFO.LT.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'PVM send error')
      ENDIF
C
      CALL CRDPUT (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     1   ROTA)
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
