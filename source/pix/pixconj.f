C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixconj.f	1.3    11/7/90
C
      SUBROUTINE PIXCONJ (A, N)
C
CD Complex conjugate of an array
C
C
C	A	CMPLX	input	Complex array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      COMPLEX		A(*)
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         A(I) = CONJG (A(I))
 10   CONTINUE
C
      CONTINUE
      END
