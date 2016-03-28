C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrpois.f	1.3    11/7/90
C
      SUBROUTINE PIXRPOIS (AIN, AOUT, N, SEED)
C
CD Add Poission noise to an array
C
C
C	AIN	REAL	input	Real array
C	AOUT	REAL	input	Real array
C	N	INT	input	Number of elements
C	SEED	INT	input	Seed for random numbers
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		AIN(*), AOUT(*)
      INTEGER		N, SEED
C
      INTEGER		I, NEVENTS
C=====================================================================
      DO 10 I = 1, N
         CALL UTLPOISS (AIN(I), NEVENTS, SEED)
         AOUT(I) = FLOAT(NEVENTS)
 10   CONTINUE
C
      CONTINUE
      END
