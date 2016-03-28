C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filopen.f	1.3    11/7/90
C
      SUBROUTINE FILOPEN (NAME, FILENAME, ACCESS)
C
CD Open file with name NAME for ACCESS
C
C
C	NAME	CH*(*)	input	NAME of file as specified to user
C	FILENAME	CH*(*)	input FILENAME
C	ACCESS	CH*(*)	input	Access mode
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, ACCESS
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILOPEN')
C
      CHARACTER		FILESYS*(SYSMXNAM),
     2			STRM2*(SYSMXNAM)
      INTEGER		NDUMMY
C
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Check for null file name
C
      IF (FILENAME.EQ.' ') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Null file name')
         GO TO 999
      END IF
C
      CALL FILSYSEX (FILENAME, FILESYS)
C
C Now create the directory entry for this file
C
      CALL DATCREAT (NAME)
C
C Now open file and read in header
C
      IF (FILESYS.EQ.'FTS') THEN
         CALL DATPUTC (NAME,'FILNAME', FILENAME, 1)
         CALL DATPUTC (NAME,'FILACCESS', ACCESS, 1)
         CALL DATPUTC (NAME,'FILSYS', FILESYS, 1)
         CALL FTSOPEN (NAME)
      ELSE IF (FILESYS.EQ.'SDE') THEN
         CALL DATREAD (NAME, FILENAME)
         CALL DATPUTC (NAME,'FILNAME', FILENAME, 1)
         CALL DATPUTC (NAME,'FILACCESS', ACCESS, 1)
         CALL DATPUTC (NAME,'FILSYS', FILESYS, 1)
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
