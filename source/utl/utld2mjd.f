C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utld2mjd.f	1.2	 7/20/92
C
      SUBROUTINE UTLD2MJD (DATE, MJD)
C
C Converts date of form 90MAY17 to Modified Julian day number
C
C	DATE	CHAR*(*)	input	Date string
C	MJD	DBLE		output	MJD
C
C Audit trail:
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DATE
      DOUBLE PRECISION  MJD
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLD2MJD')
C
      INTEGER		IY, IM, ID, J
      CHARACTER*(3)	MONTHS(12)
      DATA		MONTHS / 'JAN', 'FEB', 'MAR', 'APR', 'MAY',
     $   'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
C===================================================================
C
      IF (ERROR) GO TO 999
C
      READ (DATE(1:2), '(I2)', ERR=11) IY
      IY =  IY + 1900
      DO 10 IM = 1, 12
         IF(DATE(3:5).EQ.MONTHS(IM)) GO TO 11
 10   CONTINUE
      GO TO 900
 11   CONTINUE
      READ (DATE(6:7), '(I2)', ERR=900) ID
      CALL SLACALDJ (IY, IM, ID, MJD, J)
      IF(J.EQ.0) GO TO 990
C
 900  CONTINUE
      CALL ERRREPOR (ERRBDARG, ROUTINE, 'Invalid date')
      GO TO 999
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END




