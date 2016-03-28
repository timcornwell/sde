C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxwchi.f	1.2    11/7/90
C
      SUBROUTINE PIXXWCHI (A, W, N, CHISQ, SUMWT)
C
CD Find chisq.
C
C	A	COMPLEX	input	Input array
C	W	REAL	input	Weights
C	N	INT	input	Number of elements
C	CHISQ	REAL	output	Chi-squared
C	SUMWT	REAL	output	Sum of weights
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Removed calculation of grad(CHISQ)
C				T.J.Cornwell	May 31 1990
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		W(*), CHISQ, SUMWT
      COMPLEX		A(*)
      INTEGER		N
C
      INTEGER		I
C=====================================================================
      IF (ERROR) GO TO 999
      CHISQ = 0.0
      SUMWT = 0.0
      DO 10 I = 1, N
         IF (W(I).GT.0.0) THEN
            CHISQ = CHISQ + W(I)*ABS(A(I))**2
            SUMWT = SUMWT + W(I)
         END IF
 10   CONTINUE
C
 999  CONTINUE
      END
