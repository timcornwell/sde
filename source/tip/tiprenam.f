C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tiprenam.f	1.1    3/26/93
C
      SUBROUTINE TIPRENAM (DBSUB)
C
CD We update the NewFileName in a subdirectory of a Tipper dataBase
C
C	DBSUB	CH*(*)	inp	Tipper DataBase Subdirectory
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
      CHARACTER*(*)	DBSUB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPRENAM')
C
      CHARACTER*(SYSMXNAM)	NAMEIN, NAMEOUT
      INTEGER			VERSION, IPLUS, NDUMMY, NLEN
C
      INTEGER			STRLEN
      CHARACTER*(SYSMXNAM)	STRRMBL
      CHARACTER*2		STRINT
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETC (DBSUB, 'NewFileName', NAMEIN, 1, NDUMMY)
      IF (ERROR) GOTO 990
      NLEN = STRLEN (NAMEIN)
      IPLUS = INDEX(NAMEIN, '+')
      IF (IPLUS .EQ. 0) THEN
         NAMEOUT = NAMEIN(1:NLEN)//'+1'
      ELSE
         READ (NAMEIN(IPLUS+1:STRLEN(NAMEIN)), *) VERSION
         VERSION = VERSION + 1
         NAMEOUT = STRRMBL(NAMEIN(1:IPLUS)//STRINT(VERSION))
      ENDIF
      CALL DATPUTC (DBSUB, 'NewFileName', NAMEOUT, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
