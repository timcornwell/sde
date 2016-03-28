C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrnear.f	1.1	 4/22/93
C
      INTEGER FUNCTION PIXRNEAR (ARR, N, TARGET)
C
CD Return the index I of ARR for which ARR(I) is closest to TARGET
C
C	ARR	REAL	input	Real array
C	N	INT	input	Number of elements
C	TARGET	REAL	input	Target value for ARR
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	25 Jan 1992
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		ARR(*), TARGET
      INTEGER		N
C
      INTEGER		I
      REAL		DELTA
C=====================================================================
      PIXRNEAR = 0
      DELTA = 9E+30
      DO 10 I = 1, N
         IF ( ABS(ARR(I) - TARGET) .LT. DELTA ) THEN
            PIXRNEAR = I
            DELTA  =  ABS(ARR(I) - TARGET)
         ENDIF
 10   CONTINUE
C
      END
