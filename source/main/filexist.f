C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filexist.f	1.4    11/7/90
C
      LOGICAL FUNCTION FILEXIST (FILENAME)
C
CD Check to see if a file exists
C
C
C	FILENAME	CH*(*)	input	Name of file
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	FILENAME
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'FILEXIST')
C
      CHARACTER*(SYSMXNAM)	TFILENAM
      LOGICAL		TEXIST
C=======================================================================
      FILEXIST = .FALSE.
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (FILENAME.EQ.' ') THEN
         FILEXIST = .FALSE.
      ELSE
         CALL SYSTRANS (FILENAME, TFILENAM)
         INQUIRE (FILE = TFILENAM, EXIST = TEXIST)
         FILEXIST = TEXIST
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
