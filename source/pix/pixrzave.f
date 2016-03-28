C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrzave.f	1.1    3/20/91
C
      SUBROUTINE PIXRZAVE (ARR, NT, AVE, RMS)
C
CD Determines the average of all non-zero pixels
C
C	IN	REAL	input	Real, 2-D array to fit to
C	NT	INT	input	size of array
C	AVE	REAL	output	AVE of non-zero pixels
C	RMS	REAL	output	RMS of non-zero pixels
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 5 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	NT
      REAL	AVE, RMS, ARR(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRZAVE')
C
      INTEGER	I, N
      REAL	SUM
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      SUM = 0.0
      RMS = 0.0
      N = 0
      DO 100 I = 1, NT
         IF (ARR(I) .NE. 0.0) THEN
            N = N + 1
            SUM = SUM + ARR(I)
            RMS = RMS + ARR(I) * ARR(I)
         ENDIF
 100  CONTINUE
      IF (N .EQ. 0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No Pixels to average')
         GOTO 990
      ELSE
         AVE = SUM / FLOAT (N)
         RMS = SQRT ( RMS / FLOAT (N) )
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
