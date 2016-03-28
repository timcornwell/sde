C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixphfix.f	1.1    10/29/91
C
      SUBROUTINE PIXPHFIX (IN, OUT, N, STRIDE)
C
CD Fixup phases of an array to minimize discontinuities
C
C	IN	REAL	input	Input array  (degrees)
C	OUT	REAL	output	Output array
C	N	INT	input	Number of elements
C	STRIDE	INT	input	Stride in input and output arrays
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S. Briggs	October 23 1991
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		N, STRIDE
      REAL		IN(*), OUT(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXPHFIX')
C
      INTEGER		I, TURNS, IEND
C=========================================================================
      IF (ERROR) GO TO 999
C
      OUT(1) = IN(1)
      IEND = (N-1)*STRIDE + 1
      DO 100 I = STRIDE+1, IEND, STRIDE
         TURNS = (IN(I) - OUT(I-STRIDE) + 180.0) / 360.0
         IF (TURNS.LT.0) TURNS = TURNS - 1
         OUT(I) = IN(I) - TURNS*360.0
 100  CONTINUE
C
 999  CONTINUE
      END
