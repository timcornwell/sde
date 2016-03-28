C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixcxinv.f	1.3    11/7/90
C
      SUBROUTINE PIXCXINV (A1, MINA, A2, N)
C
CD Invert an array
C
C	A1	COMPLEX	input	COMPLEX array
C	MINA	REAL	input	Min value for A1
C	A2	COMPLEX	input	COMPLEX array
C	N	INT	input	Number of elements
C Audit trail:
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      COMPLEX		A1(*), A2(*)
      REAL		MINA
      INTEGER		N
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         IF (ABS(A1(I)).GT.MINA) THEN
            A2(I) = 1.0 / A1(I)
         ELSE
            A2(I) = 0.0
         END IF
 10   CONTINUE
C
      CONTINUE
      END
