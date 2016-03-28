C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrabsh.f	1.3    11/7/90
C
      SUBROUTINE PIXRABSH (A, NB, HIST, N, BASE, INCR)
C
CD Form a histogram of the absolute value of an array: an element (I) 
C of the histogram is centered at BASE + (I-1) * INCR
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
      PARAMETER		(ROUTINE = 'PIXRABSH')
C
      INTEGER		I, INDEX
      REAL		AMAX, AMIN
C=====================================================================
      IF (ERROR) GO TO 999
      AMAX = ABS(A(1))
      AMIN = AMAX
      DO 10 I = 1, N
         AMAX = MAX(AMAX, ABS(A(I)))
         AMIN = MIN(AMIN, ABS(A(I)))
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
      BASE = AMIN
      INCR = (AMAX-AMIN)/(NB-1)
      DO 30 I = 1, N
         INDEX = NINT((ABS(A(I))-BASE)/INCR)
         HIST(INDEX) = HIST(INDEX) + 1
  30  CONTINUE
C
  999 CONTINUE
      END
