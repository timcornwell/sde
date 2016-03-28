C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrprob.f	1.4	 7/20/92
C
      SUBROUTINE PIXRPROB (DRT, DPRED, N, PROB)
C
C Find log probability for poisson images: to a good approximation
C this is just - chi-squared/2. It is also called the Phi-divergence.
C Note that the two last terms in the Stirling approximation for N!
C are dropped.
C
C Audit trail:
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		DRT(*), DPRED(*), PROB
      INTEGER		N
C
      INTEGER		I
C=====================================================================
      IF (ERROR) GO TO 999
      PROB = 0.0      
      DO 10 I = 1, N
         IF ((DRT(I).GT.0.0).AND.(DPRED(I).GT.0.0)) THEN
            PROB = PROB + DRT(I)*LOG(DPRED(I)/DRT(I)) +
     $         DRT(I) - DPRED(I) 
         END IF
 10   CONTINUE
C
 999  CONTINUE
      END
