C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixlint.f	1.3    11/7/90
C
      SUBROUTINE PIXLINT (A, N, X, VAL)
C
CD Lagrangian interpolation routine.
C
C
C	A	REAL	input	Array of values
C	N	INTEGER	input	Number of values in A
C	X	REAL	input	Place to interpolate to.
C	VAL	REAL	output	Interpolated value
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		N
      REAL		A(N), X, VAL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXLINT')
C
      INTEGER		I, IORD
      REAL		TOP, BOTTOM
C=====================================================================
      VAL = 0.0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (FLOAT(NINT(X)).EQ.X) THEN
         VAL = A(NINT(X))
      ELSE
         VAL = 0.0
         DO 10 I = 1, N
            TOP = 1.0
            BOTTOM = 1.0
            DO 20 IORD = 1, N
               IF (I.NE.IORD) THEN
                  TOP = TOP * (X - FLOAT(IORD))
                  BOTTOM = BOTTOM * (FLOAT(I) - FLOAT(IORD))
               END IF
  20        CONTINUE
            VAL = VAL + A(I) * TOP / BOTTOM
  10     CONTINUE
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
