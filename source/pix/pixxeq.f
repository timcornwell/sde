C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxeq.f	1.1    12/11/92
C
      LOGICAL FUNCTION PIXXEQ (A1, A2, N)
C
CD Return true if arrays are exactly equal
C
C	A1	CMPLX	input	Complex array
C	A2	CMPLX	input	Complex array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version:
C				D.S.Briggs	Nov 30 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      COMPLEX		A1(*), A2(*)
      INTEGER		N
C
      INTEGER		I
C=====================================================================
      PIXXEQ = .FALSE.
      DO 10 I = 1, N
         IF (A1(I).NE.A2(I)) GO TO 999
 10   CONTINUE
      PIXXEQ = .TRUE.
C
 999  CONTINUE
      END
