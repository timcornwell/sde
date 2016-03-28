C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixnbox2.f	1.3	 11/6/92
C
      SUBROUTINE PIXNBOX2 (A, WT, N, DA, A1, AX, NINT)
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
C       A1      REAL    input   First value
C       AX      REAL    input   Last value
C	NINT	INT	output	Number of intervals
C Audit trail:
C	Original version:
C				R. T. Duquet	Apr 16 1990
C	FIRST.EQ.TRUE is illegal
C				T.J. Cornwell November 6 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	N, NINT
      REAL	A(*), WT(*), DA, A1, AX
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXNBOX2')
C
      REAL		AREF, ASTART, AEND, ALAST
      INTEGER		I1, I2
      LOGICAL           FIRST
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
      ALAST = A(I1) 
      AEND = AREF + DA
      FIRST = .TRUE.
      NINT = 1
      DO 30 I2 = I1,N
         IF (I2 .EQ. N .OR. A(I2) .GE. AX) AEND = A(I2) 
         IF ((WT(I2).GT.0.0).AND.(A(I2).GE.AEND)) THEN
            IF ((A(I2) - ALAST) .GT. DA) THEN
               IF (FIRST) THEN
                  WRITE (MESSAGE, 9000) NINT
 9000             FORMAT('Interval ',I2,': ')
                  CALL STRAPPEN(MESSAGE,STRTIMC(AREF)//' to '//
     1                          STRTIMC(AEND))
                  CALL MSGPUT(MESSAGE,'I')
                  NINT = NINT + 1
               ELSE
                  IF ((ALAST - AREF) .GT. (.5 * DA)) THEN
                     WRITE (MESSAGE, 9001) NINT
 9001                FORMAT('Interval ',I2,': ')
                     CALL STRAPPEN(MESSAGE,STRTIMC(AREF)//' to '//
     1                          STRTIMC(AEND))
                     CALL MSGPUT(MESSAGE,'I')
                     NINT = NINT + 1
                  END IF
               END IF
               AEND = A(I2)
               FIRST = .TRUE.
            ELSE
               WRITE (MESSAGE, 9001) NINT
               CALL STRAPPEN(MESSAGE,STRTIMC(AREF)//' to '//
     1                          STRTIMC(AEND))
               CALL MSGPUT(MESSAGE,'I')
               NINT = NINT + 1
               FIRST = .FALSE.
            END IF
            AREF = AEND
            AEND = AREF + DA
         END IF
         IF (WT(I2) .GT. 0.0) ALAST = A(I2)
         IF (A(I2) .GE. AX) GOTO 50
  30  CONTINUE
C
C Can jump to here if an error found
C
 50   CONTINUE
      IF (NINT .GT. 1) NINT = NINT - 1
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END


