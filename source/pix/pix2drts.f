C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drts.f	1.1    5/22/92
C
      SUBROUTINE PIX2DRTS (RES, BOX, TRIM, NX, NY, MAXRES, PX, PY)
C
CD Select image pixels based on trim level.  2-D real only.

C	RES	REAL(*)	in/out	Residual image
C	BOX	REAL(*) input	Box function
C	TRIM	REAL	input	Fractional Trim Level
C	NX	INT	input	Number of pixels in x axis
C	NY	INT	input	Number of pixels in y axis
C	MAXRES	REAL	output	Maximum residual
C	PX	INT	output	X coordinate of maximum residual
C	PY	INT	output	Y coordinate of maximum residual
C Audit trail:
C	Original version:
C				D.S.Briggs	Mar 2 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, PX, PY
      REAL		RES(NX,NY), BOX(NX,NY), TRIM, MAXRES
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DRTS')
C
      INTEGER		IPEAK, IX, IY
      REAL		TRIMVAL
C
      INTEGER		PIXISAMA
C=====================================================================
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Multiply residual by box function
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
            RES(IX,IY) = RES(IX,IY) * BOX(IX, IY)
  1      CONTINUE
  2   CONTINUE
C
C Find peak of boxed res
C
      IPEAK = PIXISAMA(NX*NY, RES, 1) - 1
      PX = 1 + MOD(IPEAK, NX)
      PY = 1 + (IPEAK - PX + 1)/NX
      MAXRES = RES(PX,PY)
      TRIMVAL = ABS(MAXRES * TRIM)
C
C Zero out all pixels below the trim value
C
      DO 11 IX = 1, NX
         DO 10 IY = 1, NY
            IF (ABS(RES(IX,IY)).LT.TRIMVAL) RES(IX,IY) = 0.0
 10      CONTINUE
 11   CONTINUE
C
 999  CONTINUE
      END
