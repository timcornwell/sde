C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlquadf.f	1.1    3/19/91
C
      SUBROUTINE UTLQUADF (A, B, C, R1, R2)
C
CD Solves the quardatic formula
C
C	NAME	CH*(*)	input	Name of directory entry
C	A	DOUBLE	input	Ax^2 + Bx + B = 0
C	B	DOUBLE	input	Ax^2 + Bx + B = 0
C	C	DOUBLE	input	Ax^2 + Bx + B = 0
C	R1	DOUBLE	output	Root 1
C	R2	DOUBLE	output	Root 2
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Feb 26 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      DOUBLE PRECISION	A, B, C
      DOUBLE PRECISION	R1, R2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLQUADF')
C
      DOUBLE PRECISION	D
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (A .EQ. 0.D0) THEN
         IF (B .EQ. 0.D0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     $         'A and B are 0.0, no solutions')
            GOTO 990
         ENDIF
         R1 = - C / B
         R2 = - C / B
      ELSE
         D = B**2 - 4 * A * C
         IF (D .LT. 0.D0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     $         'No Real Solutions')
            GOTO 990
         ELSE IF (D .EQ. 0.D0) THEN
            R1 = -B / (2.D0 * A)
            R2 = -B / (2.D0 * A)
         ELSE
            R1 = ( -B + SQRT ( D ) ) / (2.D0 * A)
            R2 = ( -B - SQRT ( D ) ) / (2.D0 * A)
         ENDIF
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
