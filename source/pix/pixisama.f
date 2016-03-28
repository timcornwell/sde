C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixisama.f	1.3    11/7/90
C
      INTEGER FUNCTION PIXISAMA (N, A, I)
C
CD Return location of absolute maximum
C
C
C	N	INT	input	Number of elements
C	A(*)	REAL	input	Array
C	I	INT	input	Return this value if no max
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		I, N
      REAL		A(N)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXISAMA')
C
      INTEGER		J
      REAL		MAXVAL
#if COMP_CONVEX
      INTEGER		ISAMAX
#endif
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
#if COMP_CONVEX
      PIXISAMA = ISAMAX (N, A, I)
#else
      PIXISAMA = I
C
      MAXVAL = 0.0
      DO 10 J = 1,N
         IF (ABS(A(J)).GT.MAXVAL) THEN
            MAXVAL = ABS(A(J))
            PIXISAMA = J
         END IF
  10  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
#endif
C
  999 CONTINUE
      END
