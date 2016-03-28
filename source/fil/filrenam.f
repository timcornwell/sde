C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filrenam.f	1.7    7/18/97
C
      SUBROUTINE FILRENAM (A, B)
C
CD Rename a file
C
C
C	A	CH*(*)	input	Name of file
C	B	CH*(*)	input	Name of file
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Fixed String concatenations
C				T.J.Cornwell	Jan 6 1994
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	A, B
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'FILRENAM')
C
      CHARACTER*(SYSMXNAM)	TFILENAM, TCMD
      INTEGER		STRLEN
      LOGICAL		TEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (A.NE.' ') THEN
         CALL SYSTRANS (A, TFILENAM)
         INQUIRE (FILE = TFILENAM, EXIST = TEXIST)
         IF(TEXIST) THEN
            TCMD = 'mv '
            CALL STRAPPEN (TCMD, A)
            CALL STRAPPEN (TCMD, ' ')
            CALL STRAPPEN (TCMD, B)
            CALL SYSSYSTM (TCMD)
         ELSE
            MESSAGE = 'File does not exist'
            CALL ERRREPOR (ERROPEN, ROUTINE, MESSAGE)
            GO TO 999
         END IF
      ELSE
         CALL ERRREPOR (ERROPEN, ROUTINE, 'Null file name')
         GO TO 999
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
