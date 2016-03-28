C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxabs.f	1.3    11/7/90
C
      SUBROUTINE PIXXABS (IN, OUT, N)
C
CD Copy an array with increments
C
C
C	IN	CMPLX	input	Input array
C	OUT	REAL	output	Output array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Feb 10 1989
C
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      COMPLEX	IN(*)
      REAL	OUT(*)
      INTEGER	N
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXXABS')
C
      INTEGER	I
C=========================================================================
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
         OUT (I) = ABS(IN (I))
  10  CONTINUE
C
 999  CONTINUE
      END
