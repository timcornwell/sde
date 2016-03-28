C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrclp2.f	1.1	 8/16/91
C
      SUBROUTINE PIXRCLP2 (A1, A2, AMIN1, AMIN2, AMAX1, AMAX2, A3, N)
C
CD Clip to value:   If A1(i) < AMIN1, set A3 to AMIN2;   
C                   If A1(i) > AMAX1, set A3 to AMAX2
C		    ELSE A3 = A2
C
C	A1	REAL	input	Real array
C	A2	REAL	input	Real array
C	AMIN1	REAL	input	Min allowed
C	AMIN2	REAL	input	LT AMIN1 goes to AMIN2
C	AMAX1	REAL	input	Max allowed
C	AMAX2	REAL	input	GT AMAX1 goes to AMAX2
C	A3	REAL	output	Real array
C	N	INT	output	Number of elements

C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	July 11 1991
C				Happy Partial Eclipse Day
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		A1(*), A2(*), A3(*), AMIN1, AMIN2, AMAX1, AMAX2
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         IF (A1(I) .GT. AMAX1) THEN
            A3(I) = AMAX2
         ELSE IF (A1(I) .LT. AMIN1) THEN
            A3(I) = AMIN2
         ELSE
            A3(I) = A2(I)
         ENDIF
 10   CONTINUE
C
      CONTINUE
      END
