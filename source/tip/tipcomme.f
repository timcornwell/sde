C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipcomme.f	1.1    3/26/93
C
      SUBROUTINE TIPCOMME (DB, COMMENT)
C
CD Add a comment to the comment list
C
C	DB	CH*(*)	inp	Tipper DataBase
C	COMMENT	CH*(*)	inp	Comment
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	27 Dec 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	DB, COMMENT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPCOMME')
C
      INTEGER		I, J
C
      CHARACTER*(SYSMXNAM)	DATESTR, NAME
C
      CHARACTER*(SYSMXNAM)	STRM2
      CHARACTER*3		STRINT
      LOGICAL	DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (.NOT.DATEXIST (DB)) THEN
         CALL MSGPUT (
     $      'Comment Target Directory doesn''t exist: '//DB, 'W')
         GOTO 999
      ENDIF
      CALL SYSDATET (DATESTR)
      J = 0
      DO 20 I = 0, 99
         IF (.NOT.DATEXIST (STRM2(DB, 'COMMENT'//STRINT(I)))) THEN
            J = I
            GOTO 30
         ENDIF
 20   CONTINUE
      CALL ERRREPOR (ERRBDARG, ROUTINE, 'Cannot add another comment')
      GOTO 999
C
 30   CONTINUE
      NAME = 'COMMENT'//STRINT(J)
      CALL DATPUTC (DB, NAME, COMMENT, 1)
      NAME = 'COMDATE'//STRINT(J)
      CALL DATPUTC (DB, NAME, DATESTR, 1)
C
C Can jump to here if an error found
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
