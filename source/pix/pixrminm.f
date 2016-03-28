C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrminm.f	1.1	 3/19/94
C
      SUBROUTINE PIXRMINM (A1, A2, A3, A4, N)
C
CD Linear combination of two arrays
C
C
C	A1	REAL	input	Real array
C	A2	REAL	input	Real array
C	A3	REAL	output	Real array
C	A4	REAL	output	Real array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	March 18 1994
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		A1(*), A2(*), A3(*), A4(*)
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         A3(I) = MIN(A1(I), A2(I))
         A4(I) = MAX(A1(I), A2(I))
 10   CONTINUE
C
      CONTINUE
      END
