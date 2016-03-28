C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdmult.f	1.3    11/7/90
C
      SUBROUTINE PIXDMULT (A1, A2, A3, N)
C
CD Multiply two DOUBLE PRECISION arrays together
C
C
C	A1	DOUBLE PRECISION	input	Real array
C	A2	DOUBLE PRECISION	input	Real array
C	A3	DOUBLE PRECISION	input	Real array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      DOUBLE PRECISION		A1(*), A2(*), A3(*)
      DOUBLE PRECISION		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         A3(I) = A1(I) * A2(I)
 10   CONTINUE
C
      CONTINUE
      END
