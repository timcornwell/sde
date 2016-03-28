C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlrand.f	1.2	 7/20/92
C
      SUBROUTINE UTLRAND(X, SEED)
C
C Make a uniformly distributed variable
C
C
C	X	REAL	output	Random number
C	SEED	INT	i/o	Random number seed
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	X
      INTEGER	SEED
C
      REAL	SUNIF
C-----------------------------------------------------------------------
C
C Call ACM routine
C
      X = SUNIF(SEED)
C
      END

