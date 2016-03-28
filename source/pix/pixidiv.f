C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixidiv.f	1.3    11/7/90
C
      SUBROUTINE PIXIDIV (A1, A2, A3, N)
C
CD Divide two INTEGER arrays together
C
C
C	A1	INTEGER	input	Real array
C	A2	INTEGER	input	Real array
C	A3	INTEGER	input	Real array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		A1(*), A2(*), A3(*)
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         IF (A2(I).NE.0) THEN
            A3(I) = A1(I) / A2(I)
         ELSE
            A3(I) = 0
         END IF
 10   CONTINUE
C
      CONTINUE
      END
