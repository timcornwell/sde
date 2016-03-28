C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrseta.f	1.2    11/26/91
C
      SUBROUTINE PIXRSETA (ARR, MASK, NT, SAVE, SRMS, AMIN)
C
CD Determines the average of all non-zero pixels
C
C	ARR	REAL	in/out	Real, array to fit to
C	MASK	REAL	input	Real, masking array
C	NT	INT	input	size of array
C	SAVE	REAL	input	AVE of non-zero pixels
C	SRMS	REAL	input	RMS of non-zero pixels
C	AMIN	REAL	input	Do not touch ARR(I) for MASK(I) < AMIN 
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 5 1991
C	Added masking array
C				M.A.Holdaway	Nov 26 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	NT
      REAL	SAVE, SRMS, AMIN, ARR(*), MASK(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRSETA')
C
      INTEGER	I, N
      REAL	SUM, AVE, RMS, TRMS, SHIFT, SCALE
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
         IF ( MASK(I) .GT. AMIN) THEN
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
         RMS = SQRT ( RMS / FLOAT (N-1) )
      ENDIF
      TRMS = SQRT (RMS**2 - AVE**2)
C
C Set Ave to SAVE, Set RMS to SRMS
C
      SHIFT = SAVE - AVE
      SCALE = SRMS / TRMS
      SUM = 0.0
      RMS = 0.0
      DO 200 I = 1, NT
         IF ( MASK(I) .GT. AMIN) THEN
            ARR(I) = (ARR(I) + SHIFT) * SCALE
            SUM = SUM + ARR(I)
            RMS = RMS + ARR(I) * ARR(I)
         ENDIF
 200  CONTINUE
      AVE = SUM / FLOAT (N)
      RMS = SQRT ( RMS / FLOAT (N-1) )
      WRITE (MESSAGE, 1111) AVE, RMS
 1111 FORMAT ('Reset array to have AVE, RMS = ',2(E14.7, 2X))
      CALL MSGPUT (MESSAGE, 'D')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
