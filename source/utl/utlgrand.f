C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlgrand.f	1.2	 7/20/92
C
      SUBROUTINE UTLGRAND(X, SEED)
C
C Make a normally distributed variable N(0,1)
C
C
C	X	REAL	output	Random number
C	SEED	INT	i/o	Random number seed
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL 	X
      INTEGER	SEED
C
      REAL	SNORM
C-----------------------------------------------------------------------
C
C Call ACM routine
C
      X = SNORM(SEED)
C
      END

