C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftx.f	1.4    11/8/90
C
#ifdef COMP_CONVEX
      SUBROUTINE FFTX (X, WORK, N, ISIGN)
C-----------------------------------------------------------------------
CD   The following calls the CONVEX VECLIB routine FFTL to perform
C   an in-place complex FFT, which is normalised when inverting.
C    Inputs:
C      X     C*4  The input signal to be transformed.
C      N     I*4  Number of points in X (N = 2**GAMMA).
C      ISIGN I*4  1 if forward fft and -1 if inverse fft.
C                 Inverse fft results will be normalized by 1/N.
C      WORK  C*4  Work array the same size as X, not used in this version
C    Outputs:
C      X     C*4  The result in normal order.
C      WORK  C*4  Work array the same size as X
C-----------------------------------------------------------------------
C                                       Declare arguments & parameters
      COMPLEX   X(*), WORK(*)
C
      INTEGER N, ISIGN
      INTEGER GAMMA
      REAL LOOKUP(16384)
      INTEGER OLDN
      DATA OLDN /0/
      SAVE OLDN
C=======================================================================
      IF (N.NE.OLDN) THEN
         OLDN = N
         CALL C1DFFT(X, N, LOOKUP, -3, IER)
      ENDIF
      CALL C1DFFT (X, N, LOOKUP, ISIGN, IER)
C
      END
#else
#if COMP_CRAY
      SUBROUTINE FFTX (X, WORK, M, ISIG)
C
C Complex to complex FFT: CRAY2 version. Currently will handle up to
C 8192.
C
C     X(2,M)  REAL  Complex array to be transformed.
C     W(2,M)  REAL  Complex work array.
C     M       INT  Length of X (must be a power of 2)
C     ISIG    INT  Direction of the transform - same convention as FPS
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
      COMPLEX X(*), WORK(*)
      INTEGER M, ISIG
C
      COMPLEX LOOKUP(20480)
      INTEGER I, OLDM
      REAL    FACT
      DATA OLDM /0/
C=======================================================================
      IF (M.NE.OLDM) THEN
         OLDM = M
         CALL CFFT2(1, ISIG, M, X, LOOKUP, WORK)
      ENDIF
      IF (ISIG.GT.0) THEN
         CALL CFFT2(0, -ISIG, M, X, LOOKUP, WORK)
         DO 10 I = 1, M
            X(I) = WORK(I)
  10     CONTINUE
      ELSE
         CALL CFFT2(0, -ISIG, M, X, LOOKUP, WORK)
         FACT = 1/FLOAT(M)
         DO 20 I = 1, M
            X(I) = WORK(I) * FACT
  20     CONTINUE
      END IF         
C
      END
#else
      SUBROUTINE FFTX (X, W, M, ISIG)
C
C   This program will produce a fast Fourier transform of a complex data
C   set using quarter-length tables by based on an algorithm devised by
C   William Newman, Cornell September 1, 1973.
C   Currently will handle up to M=8192
C      The file INCLUDEd should contain the compiler No-dependency
C   directive on vectorizing compilers.
C       On scalar systems the code can be simplified by
C   removing the branch to 400 and lines from label 400 to 490.
C   Modified for vector machines by W. D. Cotton, NRAO, Feb. 1986
C   Stolen from AIPS by T.J. Cornwell 1/7/86
C   Slightly sped up T.J. Cornwell 12/13/88
C
C     X(2,M)  REAL  Complex array to be transformed.
C     W(2,M)  REAL  Complex work array.
C     M       INT  Length of X (must be a power of 2)
C     ISIG    INT  Direction of the transform - same convention as FPS
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
      INTEGER M, ISIG
      INTEGER OLDM, N, I, J, L, M4, NM2, I1, K, MSTEP, ISEP,
     *   ITIMES, ISEPI1, JI, JI1, II, ISEP4
      INTEGER IBITRV(4096)
      REAL    X(2,M), W(2,M), C(2048), S(2048), XT, ARG, AR, AI, TPI
      DATA OLDM /0/
      SAVE OLDM
C-----------------------------------------------------------------------
      IF (M.EQ.OLDM) GO TO 200
C                                       Initilize tables
C                                       Find power of 2
      ARG = M
      N = (ALOG (ARG) / ALOG(2.0)) + 0.1
C                                       Fill sine & cosine tables
      M4 = M / 4
      NM2 = N - 2
      TPI = 8.0 * ATAN (1.0)
      TPI = TPI / M
      DO 10 I = 1,M4
         ARG = TPI * (I-1)
         C(I) = COS (ARG)
         S(I) = SIN (ARG)
 10      CONTINUE
C                                       Reverse bit order
      DO 50 I = 1,M4
         I1 = I - 1
         J = 0
         DO 20 K = 1,NM2
            L = I1 / 2
            J = I1 + 2 * (J-L)
            I1 = L
 20         CONTINUE
         J = J + 1
         IF (I.GE.J) GO TO 50
            XT = C(I)
            C(I) = C(J)
            C(J) = XT
            XT = S(I)
            S(I) = S(J)
            S(J) = XT
 50      CONTINUE
C                                       Bit reversal index array
      DO 100 I = 1,M
         I1 = I - 1
         J = 0
         DO 80 K = 1,N
            L = I1 / 2
            J = I1 + 2 * (J-L)
            I1 = L
 80         CONTINUE
         J = J + 1
         IBITRV(I) = J
 100     CONTINUE
C                                       Do transform
 200  OLDM = M
C                                       Copy to work vector (w/ conj.)
      IF(ISIG.EQ.-1) THEN
         DO 210 I = 1,M
            W(1,I) = X(1,I)
            W(2,I) = X(2,I)
 210     CONTINUE
      ELSE
         DO 211 I = 1,M
            W(1,I) = X(1,I)
            W(2,I) = - X(2,I)
 211     CONTINUE
      END IF
C                                       Butterfly loop
      DO 500 MSTEP = 1,N
         ISEP = 2 ** (N-MSTEP)
         ISEP4 = ISEP * 4
         ITIMES = 2 ** (MSTEP-1)
C                                       Move longer loop to inner
         IF (ITIMES.GT.ISEP) GO TO 400
         DO 390 I = 1,ITIMES
            ISEPI1 = 2 * (I-1) * ISEP
            II = (I+1) / 2
            JI1 = ISEPI1 + 1
            JI = JI1 + ISEP
            IF (I.EQ.(2*(I/2))) GO TO 260
C                                       Odd pass
#include	"cdirnodep.h"
               DO 250 J = 1,ISEP
                 AR = W(1,JI) * C(II) - W(2,JI) * S(II)
                 AI = W(1,JI) * S(II) + W(2,JI) * C(II)
                 W(1,JI) = W(1,JI1) - AR
                 W(2,JI) = W(2,JI1) - AI
                 W(1,JI1) = W(1,JI1) + AR
                 W(2,JI1) = W(2,JI1) + AI
                 JI1 = JI1 + 1
                 JI = JI + 1
 250             CONTINUE
              GO TO 390
C                                       Even pass
#include	"cdirnodep.h"
 260          DO 280 J = 1,ISEP
                 AR = W(1,JI) * S(II) + W(2,JI) * C(II)
                 AI = W(1,JI) * C(II) - W(2,JI) * S(II)
                 W(1,JI) = W(1,JI1) + AR
                 W(2,JI) = W(2,JI1) - AI
                 W(1,JI1) = W(1,JI1) - AR
                 W(2,JI1) = W(2,JI1) + AI
                 JI1 = JI1 + 1
                 JI = JI + 1
 280             CONTINUE
 390          CONTINUE
         GO TO 500
C                                       Switch order of loops
 400     DO 490 J = 1,ISEP
C                                       Odd passes
            II = 1
            JI1 = J
            JI = JI1 + ISEP
#include	"cdirnodep.h"
            DO 450 I = 1,ITIMES,2
               AR = W(1,JI) * C(II) - W(2,JI) * S(II)
               AI = W(1,JI) * S(II) + W(2,JI) * C(II)
               W(1,JI) = W(1,JI1) - AR
               W(2,JI) = W(2,JI1) - AI
               W(1,JI1) = W(1,JI1) + AR
               W(2,JI1) = W(2,JI1) + AI
               II = II + 1
               JI1 = JI1 + ISEP4
               JI = JI + ISEP4
 450           CONTINUE
C                                       Even passes
            II = 1
            JI1 = J + 2 * ISEP
            JI = JI1 + ISEP
#include	"cdirnodep.h"
            DO 480 I = 2,ITIMES,2
               AR = W(1,JI) * S(II) + W(2,JI) * C(II)
               AI = W(1,JI) * C(II) - W(2,JI) * S(II)
               W(1,JI) = W(1,JI1) + AR
               W(2,JI) = W(2,JI1) - AI
               W(1,JI1) = W(1,JI1) - AR
               W(2,JI1) = W(2,JI1) + AI
               II = II + 1
               JI1 = JI1 + ISEP4
               JI = JI + ISEP4
 480           CONTINUE
 490       CONTINUE
 500    CONTINUE
C                                       Bit reverse and conjugate back
C                                       to X
      IF (ISIG.EQ.-1) THEN
         DO 600 I = 1,M
            II = IBITRV(I)
            X(1,I) = W(1,II)
            X(2,I) = W(2,II)
 600     CONTINUE
      ELSE
         DO 601 I = 1,M
            II = IBITRV(I)
            X(1,I) = W(1,II)
            X(2,I) = - W(2,II)
 601     CONTINUE
      END IF
C
      IF (ISIG.EQ.-1) THEN
         DO 700 I = 1,M
            X(1,I) = X(1,I) / M
            X(2,I) = X(2,I) / M
 700     CONTINUE
      END IF
C
      END
#endif
#endif

