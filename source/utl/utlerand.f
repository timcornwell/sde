C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlerand.f	1.1    11/7/94
C
      SUBROUTINE UTLERAND(X, SEED)
C
C Make an exponentially distributed variable.  E(0,inf)  Mean is 1
C
C	X	REAL	output	Random number
C	SEED	INT	i/o	Random number seed
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL 	X
      INTEGER	SEED
C
      REAL	SEXPO
C-----------------------------------------------------------------------
C
C Call ACM routine
C
      X = SEXPO(SEED)
C
      END

