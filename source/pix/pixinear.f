C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixinear.f	1.1	 4/22/93
C
      INTEGER FUNCTION PIXINEAR (ARR, N, TARGET)
C
CD Return the index I of ARR for which ARR(I) is closest to TARGET
C
C	ARR	INT	input	Real array
C	N	INT	input	Number of elements
C	TARGET	INT	input	Target value for ARR
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	25 Jan 1992
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		ARR(*), TARGET
      INTEGER		N
C
      INTEGER		I
      INTEGER		DELTA
C=====================================================================
      PIXINEAR = 0
      DELTA = 999999
      DO 10 I = 1, N
         IF ( ABS(ARR(I) - TARGET) .LT. DELTA ) THEN
            PIXINEAR = I
            DELTA  =  ABS(ARR(I) - TARGET)
         ENDIF
 10   CONTINUE
C
      END
