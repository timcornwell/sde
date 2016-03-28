C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixmdiff.f	1.1	 5/4/93
C
      SUBROUTINE PIXMDIFF (A, N, DA)
C
CD Finds the smallest increment DA between values of A(I) and A(I+1)
C
C
C	A	REAL	input	Array of real numbers in increasing
C				sequence
C	N	INT	input	Size of A
C	DA	REAL	output	Interval
C Audit trail:
C	New routine
C                               M.A.Holdaway	Aug 1 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	N
      REAL	A(*), DA
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXNBOX')
C
      REAL		DIFF
      INTEGER		I
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DA = 99999999.
      DO 10 I = 1,N-1
         DIFF =  ABS(A(I+1)-A(I))
         IF ( DIFF .LT. DA .AND. DIFF .GT. 0.0) THEN
            DA = DIFF
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
