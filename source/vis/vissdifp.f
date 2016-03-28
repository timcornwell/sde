C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissdifp.f	1.1    2/23/95
C
      SUBROUTINE VISSDIFP (VIS, WT, TIM, BAS, NVIS, NANT, TTHRESH,
     $   NDIF, SX2)
C
CD Estimate noise by successive differences  (pixel level)
C
C	VIS	CMPLX(*) input	Vis data
C	WT	REAL(*)	input	Weights
C	TIM	REAL(*)	input	Input times
C	BAS	REAL(*)	input	Baslines
C	NVIS	INT	input	Array sizes
C	NANT	INT	input	Max antenna number in the data
C	TTHRESH	REAL(*)	input	Integration times
C	NDIF	INT	output	Number of complex differences used
C	SX2	DBLE	output	Sum of square
C
C If V1 & V2 are the real or imaginary parts of two visibility samples
C close enough in time that the signal in them can be considered constant,
C then (V1 - V2) / SQRT(1/W1 + 1/W2) is a Gaussian random variable with
C standard deviation SIGMAUW and zero mean.
C
C Only successive samples on a given baselines are considered, and then
C only if their time difference is less than SDTHRESH
C
C Audit trail:
C	New routine
C				D.S.Briggs	Feb 23 1995
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NANT, NDIF
      COMPLEX		VIS(NVIS)
      REAL		WT(NVIS), TIM(NVIS), BAS(NVIS), TTHRESH
      DOUBLE PRECISION	SX2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSDIFP')
C
      INTEGER		I, J, IA1, IA2, JA1, JA2, K
      REAL		T
C=====================================================================
      IF (ERROR) GO TO 999
C
      SX2 = 0.D0
      NDIF = 0
C
      DO 210 IA2 = 2, NANT
         DO 200 IA1 = 1, IA2-1
C
C Find first vis with current target baseline
C
            DO 100 I = 1, NVIS
               IF (WT(I).LE.0.0) GO TO 100
               JA1 = NINT(BAS(I)/256.0)
               JA2 = NINT(BAS(I)-FLOAT(256*JA1))
               IF (JA1.GT.JA2) THEN
                  K = JA1
                  JA1 = JA2
                  JA2 = K
               END IF
               IF ((IA1.EQ.JA1).AND.(IA2.EQ.JA2)) GO TO 110
 100        CONTINUE
            GO TO 200
 110        CONTINUE
C
C Find all the differences on this baseline
C            
 150        CONTINUE
            DO 160 J = I+1, NVIS
               IF (WT(J).LE.0.0) GO TO 160
               JA1 = NINT(BAS(J)/256.0)
               JA2 = NINT(BAS(J)-FLOAT(256*JA1))
               IF (JA1.GT.JA2) THEN
                  K = JA1
                  JA1 = JA2
                  JA2 = K
               END IF
               IF ((IA1.EQ.JA1).AND.(IA2.EQ.JA2)) THEN
                  IF (ABS(TIM(J)-TIM(I)).LT.TTHRESH) THEN
                     NDIF = NDIF + 1
                     T = REAL(VIS(J)) - REAL(VIS(I))
                     SX2 = SX2 + T**2 / (1/WT(I) + 1/WT(J))
                     T = IMAG(VIS(J)) - IMAG(VIS(I))
                     SX2 = SX2 + T**2 / (1/WT(I) + 1/WT(J))
                  END IF
                  I = J
                  GO TO 150
               END IF
 160        CONTINUE
 200     CONTINUE
 210  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

