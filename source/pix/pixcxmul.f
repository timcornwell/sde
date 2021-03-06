C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixcxmul.f	1.3    11/7/90
C
      SUBROUTINE PIXCXMUL (A1, A2, A3, N)
C
CD Multiply two COMPLEX arrays together, using the conjugate of A2
C
C
C	A1	COMPLEX	input	Real array
C	A2	COMPLEX	input	Real array
C	A3	COMPLEX	input	Real array
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
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         A3(I) = A1(I) * CONJG(A2(I))
 10   CONTINUE
C
      CONTINUE
      END
