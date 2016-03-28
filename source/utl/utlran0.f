C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlran0.f	1.1    11/7/94
C
      SUBROUTINE UTLRAN0 (X, INSEED)
C
C Make a uniformly distributed variable.  This version is based on the
C code from Numerical Recipes, and shuffles the basic SDE UTLRAND
C
C	X	REAL	output	Random number
C	SEED	INT	input	Random number seed
C
C If seed is < 0, it is ignored, (but the random number generator had
C better have been seeded!)
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	X
      INTEGER	INSEED
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLRAN0')
C
      INTEGER	J
      REAL	DUM
C
      REAL	V(97), Y
      INTEGER	SEED
      SAVE	V, Y, SEED
C
      INTEGER	IFF
      SAVE	IFF
      DATA	IFF /0/
C-----------------------------------------------------------------------
      IF (INSEED.GE.0.OR.IFF.EQ.0) THEN
         IFF=1
         IF (INSEED.LT.0) THEN
            SEED = 12345
         ELSE
            SEED = INSEED
         END IF
         DO 11 J=1,97
            CALL UTLRAND (DUM, SEED)
 11      CONTINUE
         DO 12 J=1,97
            CALL UTLRAND (V(J), SEED)
 12      CONTINUE
         CALL UTLRAND (Y, SEED)
      ENDIF
      J=1+INT(97.*Y)
      IF (J.GT.97.OR.J.LT.1) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'UTLRAND stuffed up')
         GO TO 999
      END IF
C
      Y=V(J)
      X=Y
      CALL UTLRAND (V(J), SEED)
 999  RETURN
      END





