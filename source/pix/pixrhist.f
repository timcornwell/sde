C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrhist.f	1.3    11/7/90
C
      SUBROUTINE PIXRHIST (A, NB, HIST, N, BASE, INCR)
C
CD Form a histogram of an array: an element (I) of the histogram is
C centered at BASE + (I-1) * INCR
C
C
C	A	REAL	input	Real array
C	NB	INT	input	Number of boxs
C	HIST	INT	input	Real array
C	N	INT	input	Number of elements
C	BASE	REAL	output	BASE of array
C	INCR	REAL	output	Increment of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		A(*), BASE, INCR
      INTEGER		N, NB, HIST(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRHIST')
C
      INTEGER		I, INDEX
      REAL		AMAX, AMIN
C=====================================================================
      IF (ERROR) GO TO 999
      AMAX = A(1)
      AMIN = A(1)
      DO 10 I = 1, N
         AMAX = MAX(AMAX, A(I))
         AMIN = MIN(AMIN, A(I))
 10   CONTINUE
C
      IF (AMAX.EQ.AMIN) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Zero range in values')
         GO TO 999
      END IF
C
      DO 20 I = 1, NB
         HIST(I) = 0
  20  CONTINUE
C
      INCR = (AMAX-AMIN)/(NB-1)
      BASE = AMIN
      DO 30 I = 1, N
         INDEX = NINT((A(I)-BASE)/INCR)
         HIST(INDEX) = HIST(INDEX) + 1
  30  CONTINUE
C
  999 CONTINUE
      END
