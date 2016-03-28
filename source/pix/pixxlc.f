C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxlc.f	1.1    1/9/91
C
      SUBROUTINE PIXXLC (A1, W1, A2, W2, A3, N)
C
CD Linear combination of two complex arrays
C
C
C
C	A1	CMPLX	input	X array
C	W1	REAL	input	Weight
C	A2	CMPLX	input	X array
C	W2	REAL	input	Weight
C	A3	CMPLX	input	X array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      COMPLEX		A1(*), A2(*), A3(*)
      REAL		W1, W2
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         A3(I) = W1 * A1(I) + W2 * A2(I)
 10   CONTINUE
C
      CONTINUE
      END
