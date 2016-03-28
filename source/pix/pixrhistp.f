C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrhistp.f	1.1    11/7/90
C
      SUBROUTINE PIXRHISTP (HIST, SIZE, MEAN, TRIALS, SEED)
C
CD Calculate a histogram using Poisson Stats
C
C
C
C	hist	REAL	input	Real array to hold histogram
C	size	INT	input	size of hist
C	trials	INT	input	number of trials to do
C	seed	INT	input	ramdom number seed
C	mean	INT	input	mean of the distribution
C Audit trail:
C	Cloned from pixrcorr
C				R.G. Marson     Aug 20 1990
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		SIZE, TRIALS, SEED, MEAN
      REAL		HIST(0:SIZE-1)
C
      INTEGER		ACTUAL, I
C=====================================================================
      DO I = 1, TRIALS
         CALL UTLPOISS(MEAN, ACTUAL, SEED)
         IF (ACTUAL .LT. SIZE) HIST(ACTUAL) = HIST(ACTUAL) + 1
      END DO
C

C
      CONTINUE
      END
