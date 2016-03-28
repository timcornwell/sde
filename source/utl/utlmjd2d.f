C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlmjd2d.f	1.3    11/17/90
C
      SUBROUTINE UTLMJD2D (MJD, DATE)
C
CD Converts Modified Julian day number to date of form 90MAY17
C
C	MJD	DBLE		input	MJD
C	DATE	CHAR*(*)	output	Date string
C
C Audit trail:
C	Changed to warning only of invalid date
C				T.J. Cornwell 	17 Nov 1990
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      DOUBLE PRECISION  MJD
      CHARACTER*(*)	DATE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLMJD2D')
C
      INTEGER		IYMDF(4), J
      CHARACTER*(3)	MONTHS(12)
      DATA		MONTHS / 'JAN', 'FEB', 'MAR', 'APR', 'MAY',
     $   'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
C===================================================================
      DATE = '00JAN01'
C
      IF (ERROR) GO TO 999
C
      CALL SLADJCAL (1, MJD, IYMDF, J)
      IF(J.NE.0) GO TO 900
      WRITE (DATE, 1000) IYMDF(1)-1900, MONTHS(IYMDF(2)), IYMDF(3)
 1000 FORMAT (I2,A3,I2)
      GO TO 999
C
 900  CONTINUE
      CALL MSGPUT ('UTLMJD2D: Invalid date', 'W')
      GO TO 999
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END




