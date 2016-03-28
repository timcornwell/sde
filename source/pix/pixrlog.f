C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrlog.f	1.1	 1
C
      SUBROUTINE PIXRLOG (IN, BLANK, OUT, NT)
C
CD      OUT = LOG ( IN )
C
C	IN	REAL	input	Input array
C	BLANK	REAL	input	Value to use if IN .LE. 0.0
C	OUT	REAL	output	Output array
C	NT	REAL	input	Input array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 9 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL		IN(*), OUT(*), BLANK
      INTEGER		NT
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXRLOG')
C
      INTEGER		I
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 100 I = 1, NT
         IF (IN(I) .LE. 0.0) THEN
            OUT(I) = BLANK
         ELSE
            OUT(I) = ALOG ( IN(I) )
         ENDIF
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
