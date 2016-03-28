C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrgaus.f	1.3    11/7/90
C
      SUBROUTINE PIXRGAUS (AIN, AOUT, N, STD, SEED)
C
CD Add Gaussian noise to an array
C
C
C	AIN	REAL	input	Real array
C	AOUT	REAL	input	Real array
C	N	INT	input	Number of elements
C	STD	REAL	input	STD of noise
C	SEED	INT	input	Seed for random numbers
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		AIN(*), AOUT(*), STD
      INTEGER		N, SEED
C
      REAL		X
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         CALL UTLGRAND (X, SEED)
         AOUT(I) = AIN(I) + X * STD
 10   CONTINUE
C
      END
