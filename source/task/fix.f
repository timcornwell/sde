C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fix.f	1.3	 7/20/92
C
      SUBROUTINE SDEMAIN
#include "stdinc.h"
      INTEGER NAX, NAXIS(7)
      CHARACTER*8 ATYPE(7)
      REAL DELT(7), RPIX(7), ROTA(7)
      DOUBLE PRECISION RVAL(7)
      INTEGER N
      CALL MSGWELCO ('Special fixer')
      CALL FILIMGGE ('In', 'D0/M31.MOD', ' ')
      CALL CRDGET ('In', NAX, ATYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      NAX = 3
      NAXIS(3) = 1
      ATYPE(3) = 'FREQ'
      RPIX(3) = 1.0
      ROTA(3) = 0.0
      DELT(1) = 0.2E-3/3600.0
      DELT(2) = 0.2E-3/3600.0
      DELT(3) = 1.4E13
      RVAL(1) = 0.0
      RVAL(2) = 35.0
      RVAL(3) = 1.4E14
      CALL DATPUTC ('In', 'TELESCOP', 'NOAOA', 1)
      CALL DATPUTC ('In', 'OBSERVER', 'CORNWELL', 1)
      CALL CRDPUT ('In', NAX, ATYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL FILIMGPU ('In', 'D1/NOAOA.MOD', ' ')
      END
