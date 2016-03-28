C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trpcaln.f	1.2	 7/20/92
C
      SUBROUTINE TRPCALN (NANT, TRPARRY, TRPWT, TRPAINV, N)
C
C Find rms error in triple product. Weights must be normalized.
C
C	NANT	INT	input	Number of antennas
C	TRPARRY	CMPLX	input	Input triple product data
C	TRPWT	REAL	input	Weights
C	TRPAINV	CMPLX	output	Solved u,v data
C	N	REAL	output	Residual
C
C Audit trail:
C	Renamed from BSCALN
C				T.J.Cornwell	March 3 1989
C
C-----------------------------------------------------------------------
C
#include	"stdinc.h"
C
C Arguments
C
      INTEGER	NANT
      COMPLEX	TRPARRY(NANT,NANT,*)
      REAL	TRPWT (NANT,NANT,*)
      COMPLEX	TRPAINV(NANT,*)
      REAL	N
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPCALN')
C
C Local variables
C
      INTEGER	IA1, IA2, IA3
C-----------------------------------------------------------------------
      IF (ERROR) GO TO 999
C
      N = 0.0
      DO 30 IA3 = 1, NANT
         DO 20 IA2 = 1, NANT
            DO 10 IA1 = 1, NANT
               IF (TRPWT (IA1,IA2,IA3).GT.0.0) THEN
                  N = N + ABS(TRPAINV(IA1,IA2) * TRPAINV(IA2, IA3) *
     1               CONJG(TRPAINV(IA1,IA3)) - TRPARRY(IA1,IA2,IA3))**2 
     2               * TRPWT (IA1,IA2,IA3)
               END IF
 10         CONTINUE
 20      CONTINUE
 30   CONTINUE
C
      N = SQRT (N)
C
 999  CONTINUE
      END
