C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxsetc.f	1.3    1/25/91
C
      SUBROUTINE PIXXSETC (A, CONST, N)
C
CD Initialize an array
C
C
C	A	CMPLX	input	Array to be set
C	CONST	REAL	input	Constant
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
      REAL		CONST
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         A(I) = CMPLX (CONST, 0.)
 10   CONTINUE
C
      CONTINUE
      END
