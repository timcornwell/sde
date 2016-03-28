C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filimgre.f	1.3    11/7/90
C
      SUBROUTINE FILIMGRE (NAME)
C
CD READ image from file with name NAME
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
      PARAMETER		(ROUTINE = 'FILIMGRE')
C
      CHARACTER		FILESYS*(SYSMXNAM)
      INTEGER		NDUMMY
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Now open file and read in binary data
C
      CALL DATGETC (NAME, 'FILSYS', FILESYS, 1, NDUMMY)
      IF (FILESYS.EQ.'FTS') THEN
         CALL FTSIMGRE(NAME)
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
