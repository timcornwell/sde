C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrsqrt.f	1.1 5/6/92
C
      SUBROUTINE PIXRSQRT (IN, OUT, N)
C
CD Form sqrt
C
C
C	IN	REAL	input	Input array
C	OUT	REAL	output	Output array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Feb 10 1989
C
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	IN(*), OUT(*)
      INTEGER	N
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRSQRT')
C
      INTEGER	I
C=========================================================================
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
         IF(IN(I).GT.0.0) THEN
            OUT (I) = SQRT(IN (I))
         ELSE
            OUT(I)=0.0
         END IF
  10  CONTINUE
C
 999  CONTINUE
      END
