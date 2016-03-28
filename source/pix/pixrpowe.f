C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrpowe.f	1.2    12/24/92
C
      SUBROUTINE PIXRPOWE (IN, POWER, BLANK, OUT, NT)
C
CD Raise IN to POWER
C
C	IN	REAL	input	Input array
C	POWER	REAL	input	Power to raise to
C	BLANK	REAL	input	Value to use if POWER doesn't work
C	OUT	REAL	output	Output array
C	NT	REAL	input	Input array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	If (BLANK = -99), then we give  SIGN(IN) * (ABS(IN)) ** POWER
C				M.A.Holdaway	Dec 24 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL		IN(*), OUT(*), POWER, BLANK
      INTEGER		NT
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXRPOWE')
C
      INTEGER		I, ISIGN
      LOGICAL		PROBLEM
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (POWER .NE. NINT(POWER) .OR. POWER .LT. 0.0)  THEN
         PROBLEM = .TRUE.
      ELSE
         PROBLEM = .FALSE.
      ENDIF
C
      IF (BLANK .EQ. -99.0) THEN
         DO 100 I = 1, NT
            ISIGN = IN(I)/ABS(IN(I))
            OUT(I) = ISIGN * (ABS(IN(I))) ** POWER
 100     CONTINUE
      ELSE
         DO 200 I = 1, NT
            IF (IN(I) .LE. 0.0 .AND. PROBLEM) THEN
               OUT(I) = BLANK
            ELSE
               OUT(I) = IN(I) ** POWER
            ENDIF
 200     CONTINUE
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
