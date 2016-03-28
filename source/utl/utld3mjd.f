C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utld3mjd.f	1.1	 9/22/92
C
      SUBROUTINE UTLD3MJD (DATE, MJD)
C
C Converts date of form '26/11/92' to Modified Julian day number
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
      PARAMETER		(ROUTINE = 'UTLD3MJD')
C
      INTEGER		IY, IM, ID, J
C===================================================================
C
      IF (ERROR) GO TO 999
C
      READ (DATE(7:8), '(I2)', ERR=900) IY
      IY =  IY + 1900
      READ (DATE(4:5), '(I2)', ERR=900) IM
      READ (DATE(1:2), '(I2)', ERR=900) ID
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




