C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixclean.f	1.1 11/22/94
C
      SUBROUTINE PIXCLEAN (A, M, B, X, NITER, GAIN, LIM)
C
CD Find solution to A X = B using Clean algorithm. A *MUST* be square.
C
C	A	REAL(*)	input	Input array.
C	X	REAL(*)	output	Solution vector.
C	B	REAL(*)	input	Name of constant vector.
C
C Audit trail:
C	Initial version
C				T.J. Cornwell   November 22 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		M, NITER
      REAL		A(M, M), X(M), B(M), GAIN, LIM
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXCLEAN')
C
      INTEGER		ITER, I, J, IPEAK, PIXISAMA
      REAL		BMAX
C=====================================================================
      IF (ERROR) GO TO 999
C
C First subtract current estimate
C
      DO 5 I = 1, M
         DO 4 J = 1, M
            B(J) = B(J) - A(J, I) * X(I)
 4       CONTINUE
 5    CONTINUE
C
C Loop NITER times
C
      DO 10 ITER = 1, NITER
         IPEAK = PIXISAMA (M, B, 1)
         IF(ABS(B(IPEAK)).LT.LIM) GO TO 30
         BMAX = GAIN * B(IPEAK)
         X(IPEAK) = X(IPEAK) + BMAX
         DO 20 J = 1, M
            B(J) = B(J) - A(J, IPEAK) * BMAX
 20      CONTINUE
 10   CONTINUE
C
 30   CONTINUE
C
 999  CONTINUE
      END
