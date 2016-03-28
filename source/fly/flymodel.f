C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)flymodel.f	1.1    12/29/92
C
      SUBROUTINE FLYMODEL (ANPATCH, BITER, NITER, CCL, VIS, MSUB, TSUB,
     $   CMP, DRT, MVS, FSWITCH)
C
CD Subroutine to form model visibility from clean components.
C
C Audit trail:
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		ANPATCH, BITER, NITER
      REAL		FSWITCH
      CHARACTER*(*)	VIS, CCL, MSUB, TSUB, CMP(*), DRT(*), MVS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYMODEL')
C
      INTEGER		IP
C==================================================================
      IF(ERROR) GO TO 990
C
      CALL DATPUTI (CCL, 'BITER', BITER, 1)
      CALL DATPUTI (CCL, 'NITER', NITER, 1)
      DO 400 IP = 1, ANPATCH
         CALL DATPUTI (CCL, 'FIELD', IP, 1)
         CALL FLYCCIMG (CMP(IP), CCL)
 400  CONTINUE
      CALL FLYTOVISH(ANPATCH, VIS, MSUB, TSUB, CMP, DRT, MVS, FSWITCH)
C
C Error?
C
 990  CONTINUE
      IF(ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
