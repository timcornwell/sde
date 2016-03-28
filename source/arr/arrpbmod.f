C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrpbmod.f	1.1	 8/16/91
C
      SUBROUTINE ARRPBMOD (C, NC, PB, TELDIAM)
C
CD Creates a PB from models: PB = 1 + C(1)X^2 + C(2)X^4 + C(3)X^6
C
C	C	RE(*)	input	Model parameters
C	NC	INT	input	Number of C parameters
C	PB	CHAR(*)	input	Directory of Calculated PB
C	TELDIAM	REAL	input	Telescop Diameter, METERS
C
C				M.A.Holdaway	July 23 1991
C
C--------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NC
      CHARACTER*(*)	PB
      REAL		C(*), TELDIAM
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRPBMOD')
C
      REAL		RADMAX
      INTEGER		NPB, PBADD
C=========================================================================
      LOGICAL		DATEXIST
C=========================================================================
      IF (ERROR) GO TO 999
C
      NPB = 8192
      RADMAX = 3.30
C
      IF (DATEXIST(PB)) CALL DATDELET (PB)
C
      CALL DATMAKAR (PB, 1, NPB, 'R', PBADD)
      CALL PIXPBMOD (C, NC, RADMAX, MEMR(PBADD), NPB)
      CALL DATPUTR  (PB, 'RADMAX', RADMAX, 1)
      CALL DATPUTR  (PB, 'TELDIAM', TELDIAM, 1)
C
 999  CONTINUE
      END
