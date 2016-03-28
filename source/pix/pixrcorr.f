C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrcorr.f	1.3    11/7/90
C
      SUBROUTINE PIXRCORR (A1, S1, A2, S2, A3)
C
CD Correlate two arrays
C
CS Arguments: CALL pixrcorr (a1, s1, a2, s2, a3)
CS
CS	a1	REAL	input	Real array
CS	s1	INT	input	size of a1
CS	a2	REAL	input	Real array
CS	s2	INT	input	size of a2
CS	a3	REAL	input	Real array
CD Audit trail:
C	Cloned from pixrlc
C				R.G. Marson     Aug 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		S1, S2
      REAL		A1(S1), A2(S2), A3(S1+S2)
C
      INTEGER		I, J, TEMP1, TEMP2
C=====================================================================
      TEMP1 = 1 - S1
      DO I = 1, S1 + S2 - 1
         A3(I) = 0
         TEMP2 = S1 - I
         DO J = MAX(1, I + TEMP1), MIN(S2, I)
            A3(I) = A3(I) + A2(J) * A1(TEMP2 + J)
         END DO
      END DO
C
      CONTINUE
      END
