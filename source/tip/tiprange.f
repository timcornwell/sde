C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tiprange.f	1.1    3/26/93
C
      SUBROUTINE TIPRANGE (DB, STIME, I1, I2)
C
CD Utility to turn STIME range into I1 and I2
C
C	DB	CH*(*)	inp	Tipper DataBase
C	STIME	INT(2)	inp	Start and Stop Times
C	I1	INT	out	Index of start time
C	I2	INT	out	Index of stop time
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	23 Dec 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	DB
      INTEGER		STIME(2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPRANGE')
C
      INTEGER	NAX, NAXIS(SYSMXDIM), ADD, N, I
      CHARACTER*1	TYPE
C      
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		I1, I2, J1, J2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM2(DB, 'LIST'), NAX, NAXIS, TYPE, ADD)
      N = NAXIS(1)
      I1 = N-1
      DO 100 I = 0, N-1
         J1 = MEMI(ADD+I)
         IF (J1 .GE. STIME(1)) THEN
            I1 = I
            GOTO 200
         ENDIF
 100  CONTINUE
 200  CONTINUE
      I2 = 0
      DO 300 I = N-1, 0, -1
         J2 = MEMI(ADD+I)
         IF (J2 .LE. STIME(2)) THEN
            I2 = I
            GOTO 400
         ENDIF
 300  CONTINUE
 400  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
