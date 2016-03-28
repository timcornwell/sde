C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrgsp.f	1.3    11/7/90
C
      SUBROUTINE PIXRGSP (AIN, ACCEL, AOUT, N)
C
CD Clip an array
C
C
C	AIN	REAL	input	Real array
C	ACCEL	REAL	input	Negative Gain
C	AOUT	REAL	input	Real array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		AIN(*), AOUT(*), ACCEL
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         IF (AIN(I).LT.0.0) THEN
            AOUT(I) = ACCEL * AIN(I)
         ELSE
            AOUT(I) = AIN(I)
         END IF
 10   CONTINUE
C
      CONTINUE
      END
