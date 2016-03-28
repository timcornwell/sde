C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hedbempu.f	1.1    8/4/94
C
      SUBROUTINE HEDBEMPU (IMG, BEAM, UNIT, PPB)
C
CD Put beam/taper parameters into header
C
C	IMG	CH*(*)	input	Name of image
C	BEAM	R(4)	input	BMAJ, BMIN, BPA, BZ
C	UNIT	CH*(*)	input	BUNIT
C	PPB	R	input	BPPB
C
C Audit trail:
C	Original version
C				D.S.Briggs	Aug 3 1994
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, UNIT
      REAL		BEAM(4), PPB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HEDBEMPU')
C==================================================================
      IF (ERROR) GO TO 999
C
C It's an error if these aren't present
C
      CALL DATPUTR (IMG, 'BMAJ', BEAM(1)/3600.0, 1)
      CALL DATPUTR (IMG, 'BMIN', BEAM(2)/3600.0, 1)
      CALL DATPUTR (IMG, 'BPA', BEAM(3), 1)
      CALL DATPUTR (IMG, 'BZ', BEAM(4)/3600.0, 1)
C
      IF (PPB.GE.0.0) CALL DATPUTR (IMG, 'BPPB', PPB, 1)
      IF (UNIT.NE.' ') CALL DATPUTC (IMG, 'BUNIT', UNIT, 1)
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
