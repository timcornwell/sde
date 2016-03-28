C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrflag.f	1.1    11/7/94
C
      SUBROUTINE PIXRFLAG (A1, A2, A3, N)
C
CD Flag visibility weights based on flag array
C
C	A1	REAL	input	Real array
C	A2	REAL	input	Real array
C	A3	REAL	input	Real array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		A1(*), A2(*), A3(*)
      INTEGER		N
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         IF (A2(I).LT.0.0) THEN
            A3(I) = -ABS(A1(I))
         ELSE
            A3(I) = A1(I)
         END IF
 10   CONTINUE
C
      CONTINUE
      END
