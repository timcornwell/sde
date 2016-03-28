C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrsum.f	1.1	 3/15/91
C
      SUBROUTINE PIXRSUM (A1, N, SUM)
C
CD Sums the elements of an array
C
C Arguments: CALL PIXRSUM (A1, N, SUM)
C
C	A1	REAL	input	Real array
C	N	INT	input	Number of elements
C	SUM	REAL	output	Sum of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 12 1991
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		A1(*), SUM
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      SUM = 0.
      DO 10 I = 1, N
         SUM = SUM + A1(I)
 10   CONTINUE
C
      CONTINUE
      END
