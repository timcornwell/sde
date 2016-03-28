C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlpoiss.f	1.2	 7/20/92
C
      SUBROUTINE UTLPOISS(MEAN, NEVENTS, SEED)
C
C Make a poisson distributed variable
C
C
C	MEAN	REAL	input	Mean number of events
C	NEVENTS	INT	output	Actual number of events
C	SEED	INT	i/o	Random number seed
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL 	MEAN
      INTEGER	NEVENTS
      INTEGER	SEED
C
      INTEGER	KPOISS
C-----------------------------------------------------------------------
      NEVENTS = 0
C
      IF (MEAN.LE.0.0) RETURN
C
C Call ACM routine
C
      NEVENTS = KPOISS(SEED, MEAN)
C
      END
