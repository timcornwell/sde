C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filclose.f	1.3    11/7/90
C
      SUBROUTINE FILCLOSE (NAME)
C
CD Close file with name NAME 
C
C
C	NAME	CH*(*)	input	NAME of file as specified to user
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILCLOSE')
C
      CHARACTER		FILENAME*(SYSMXNAM),
     1			FILESYS*(SYSMXNAM),
     2			STRM2*(SYSMXNAM)
C
      INTEGER		NDUMMY
C
C==================================================================
C
      IF (ERROR) GO TO 999
C
      CALL DATGETC (NAME,'FILSYS', FILESYS, 1, NDUMMY)
C
      IF (FILESYS.EQ.'FTS') THEN
         CALL FTSCLOSE (NAME)
      ELSE IF (FILESYS.EQ.'SDE') THEN
      ELSE
         MESSAGE = 'File system: '//FILESYS//' not supported'
         CALL ERRREPOR (ERRWRGTP, ROUTINE, MESSAGE)
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
