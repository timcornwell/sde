C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixnbox.f	1.5    12/14/90
C
      SUBROUTINE PIXNBOX (A, WT, N, DA, NINT)
C
CD Finds the number of distinct DA intervals in A
C
C
C	A	REAL	input	Array of real numbers in increasing
C				sequence
C	N	INT	input	Size of A
C	DA	REAL	input	Interval
C	NINT	INT	output	Number of intervals
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed GT to GE in test for integrations 
C				R.Braun 	Jul 9 1989
C       Added DASMALL to remove rounding errors
C                               R.G. Marson     Dec 10 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	N, NINT
      REAL	A(*), WT(*), DA
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXNBOX')
C
      REAL		AREF, DASMALL
      INTEGER		I
C=======================================================================
C
      NINT = 1
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DASMALL = DA - DA*5.0E-5
      AREF = A(1)
      DO 10 I = 1,N
         IF ((WT(I).GT.0.0).AND.(ABS(A(I)-AREF).GE.DASMALL)) THEN
            NINT = NINT + 1
            AREF = A(I)
         END IF
  10  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
