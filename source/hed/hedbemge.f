C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hedbemge.f	1.1    8/4/94
C
      SUBROUTINE HEDBEMGE (IMG, BEAM, UNIT, PPB)
C
CD Get beam/taper parameters from header
C
C	IMG	CH*(*)	input	Name of image
C	BEAM	R(4)	output	BMAJ, BMIN, BPA, BZ
C	UNIT	CH*(*)	output	BUNIT
C	PPB	R	output	BPPB
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
      PARAMETER		(ROUTINE = 'HEDBEMGE')
C
      INTEGER		NDUMMY
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST
      REAL		DATFGETR
C==================================================================
      IF (ERROR) GO TO 999
C
C It's an error if these aren't present
C
      BEAM(1) =  DATFGETR(IMG, 'BMAJ')*3600.0
      BEAM(2) =  DATFGETR(IMG, 'BMIN')*3600.0
      BEAM(3) =  DATFGETR(IMG, 'BPA')
      BEAM(4) =  DATFGETR(IMG, 'BZ')*3600.0
C
      IF (DATEXIST(STRM2(IMG,'BPPB')).AND.(PPB.GE.0.0))
     $   PPB = DATFGETR(IMG, 'BPPB')
C
      IF (DATEXIST(STRM2(IMG,'BUNIT')).AND.(LEN(UNIT).GT.1))
     $   CALL DATGETC (IMG,'BUNIT', UNIT, 1, NDUMMY)
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
