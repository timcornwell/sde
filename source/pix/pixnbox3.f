C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixnbox3.f	1.2	 5/15/92
C
      SUBROUTINE PIXNBOX3 (A, WT, N, DA, A1, AINT, AX, NINT)
C
C Finds the number of distinct DA intervals in A.
C This version starts at A1 and leaves off at A >= AX.
C It also DOES NOT use the absolute value for differences.
C
C
C	A	REAL	input	Array of real numbers in increasing
C				sequence
C	N	INT	input	Size of A
C	DA	REAL	input	Interval
C       A1      REAL    in/out  First value
C       AINT    REAL    output  Interval 
C       AX      REAL    in/out  Last value
C       NINT    INT     output  Number of intervals
C Audit trail:
C	Original version:
C				R. T. Duquet	Apr 16 1990
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	N
      REAL	A(*), WT(*), DA, A1, AX
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXNBOX3')
C
      REAL		AREF, AINT, ALAST, AINCR, AEND
      INTEGER		I1, I2, NINT
C
      CHARACTER*12      STRTIMC     
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I1 = 1,N
         IF ((WT(I1).GT.0.0).AND.((A(I1)-A1).GE.0)) THEN
            GO TO 20
         END IF
  10  CONTINUE
      CALL ERRREPOR(ERRNTFND, ROUTINE, 'No data found within specs')
      GO TO 999
C
 20   AREF = A(I1)
      A1 = AREF
      ALAST = AREF 
      AEND = AREF + DA
      AINT = 0.0
      NINT = 1

      DO 30 I2 = I1,N
         IF (I2 .EQ. N .OR. A(I2) .GE. AX) AEND = A(I2)
         IF (A(I2) .GE. AEND) THEN
            IF ((A(I2) - ALAST) .GT. DA) THEN
               GO TO 100
            ELSE IF (A(I2) .GE. AX) THEN
               GO TO 100
            ELSE
               NINT = NINT + 1
            END IF
            AREF = AEND
            AEND = AREF + DA
         END IF
         ALAST = A(I2)
  30  CONTINUE

C
C Now figure out an even increment
C
 100  IF ((ALAST - AREF) .LT. (.5 * DA)) NINT = NINT - 1
      IF (NINT .LE. 0) NINT = 1
      AX = ALAST 
      ALAST = ALAST + 1.0/(3600.0*24.0)
      AINT = (ALAST - A1) / NINT
C
C Can jump to here if an error found
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END




