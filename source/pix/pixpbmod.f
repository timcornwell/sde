C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixpbmod.f	1.1	 8/16/91
C
      SUBROUTINE PIXPBMOD (C, NC, RADMAX, PB, NPB)
C
CD Creates a PB from models: PB = 1 + C(1)X^2 + C(2)X^4 + C(3)X^6
C
C	C	RE(*)	input	Model parameters
C	NC	INT	input	Number of model parameters
C	RADMAX	RE	input	Max value of argument
C	PB	RE(*)	output	Calculated PB
C	NPB	INT	input	Size of PB array
C
C				M.A.Holdaway	July 23 1991
C
C--------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NC, NPB
      REAL		C(*), PB(*), RADMAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXPBMOD')
C
      REAL		RSCL, RAD, RADPOW
      INTEGER		I, J
C=========================================================================
      IF (ERROR) GO TO 999
C
      RSCL = FLOAT (NPB-1)/RADMAX
C
      DO 100 I = 1, NPB
         RAD = FLOAT(I-1)/RSCL
         PB(I) = 1.0
         RADPOW = 1.0
         DO 50 J = 1, NC
            RADPOW = RADPOW*RAD
            PB(I) = PB(I) + C(J)*RADPOW
 50      CONTINUE
      IF (PB(I) .LT. 0.0) PB(I) = 0.0
      IF (PB(I) .GT. 1.0) PB(I) = 1.0
 100  CONTINUE
C
 999  CONTINUE
      END
