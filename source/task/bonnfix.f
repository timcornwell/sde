C
C     National Radio Astronomy Observatory, Socorro, NM 87801
C     Software Development Environment (SDE)
C++
C @(#)bonnfix.f	1.2 6/1/95
C
      SUBROUTINE SDEMAIN
C
CD Fix the header of a BONN Survey image
C
C Audit trail:
C     Original version: Audit trail comments go on this line
C     and successive lines
C                                           M.A. Holdaway    June 1 1995
C---------------------------------------------------------------------
#include "stdinc.h"
      CHARACTER*(SYSMXNAM)	IN, OUT, UNITS
      INTEGER NAX, NAXIS(7)
      CHARACTER*8  TYPE(7)
      REAL DELT(7), RPIX(7), ROTA(7), BEAM(4), FREQ
      DOUBLE PRECISION RVAL(7)
      INTEGER  NDUMMY
      DATA	NAXIS / SYSMXDIM*1/
      DATA	RVAL  / SYSMXDIM*0./
      DATA	RPIX / SYSMXDIM*1./
      DATA	DELT / SYSMXDIM*0./
      DATA	ROTA /SYSMXDIM*0./
      CALL MSGWELCO ('Bonn fixer')
C
      CALL USRCTL
      CALL USRGETC ('In', IN, 1, NDUMMY)
      CALL USRGETC ('Out', OUT, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('Units', UNITS, 1, NDUMMY)
      BEAM(1) = BEAM(1)/3600.0
      BEAM(2) = BEAM(2)/3600.0
C
      CALL FILIMGGE ('In1', IN, ' ')
C
      CALL CRDGET ('In1', NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     1   ROTA)
      IF (TYPE(3) .EQ. 'LAMBDA') THEN
         RVAL(3) = 3.0E+8/RVAL(3)
         TYPE(3) = 'FREQ'
      ENDIF
      CALL CRDPUT ('In1', NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     1   ROTA)
      CALL DATPUTC ('In1', 'BUNIT', UNITS, 1)
      CALL DATPUTR ('In1', 'BMAJ', BEAM(1), 1)
      CALL DATPUTR ('In1', 'BMIN', BEAM(2), 1)
      CALL DATPUTR ('In1', 'BPA', BEAM(3), 1)
      CALL DATPUTR ('In1', 'BZ', BEAM(4), 1)
      CALL FILIMGPU ('In1', OUT, ' ')
C
C
C
      END
