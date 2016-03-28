C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filimgpu.f	1.6    12/1/94
C
      SUBROUTINE FILIMGPU (NAME, FILENAME, TBLLIST)
C
CD Put image file.
C
C	NAME	 CH*(*)	input	NAME of file as specified to user
C	FILENAME CH*(*)	input	File name
C	TBLLIST CH*(*)	input	List of tables to write
C Audit trail:
C	Added sdetype checking
C				T.J.Cornwell	Feb 3 1989
C	Added graceful check for NAME's existence
C				D.S.Briggs	Feb 26 1992
C	Added PGM file support
C				D.S.Briggs	Oct 29 1992
C	Handle case of long FILENAME comparatively gracefully.
C	You still can't use names longer than SYSMXNAM, but at least
C	now trailing blanks won't cause an error.
C				D.S.Briggs	Nov 29 1994
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, TBLLIST
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILIMGPU')
C
      CHARACTER		FILESYS*(SYSMXNAM), TMPFILE*(SYSMXNAM),
     $   		ACCESS*(SYSMXNAM),
     $			DATE*8
C
      INTEGER		STRLEN
      LOGICAL		DATEXIST
C
      DATA		ACCESS / 'WRITE' /
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
C Make certain that the directory exists
C
      IF (.NOT.DATEXIST(NAME)) THEN
         MESSAGE = 'Image ' // NAME(1:STRLEN(NAME)) // ' doesn''t exist'
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Add date of map to directory
C
      CALL SYSDATEC (DATE)
      CALL DATPUTC (NAME, 'DATE-MAP', DATE, 1)
C
      CALL FILSYSEX (FILENAME, FILESYS)
C
      IF (FILESYS.EQ.'FTS') THEN
         TMPFILE = FILENAME
         IF (TMPFILE.NE.FILENAME) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Filename too long')
            GO TO 999
         END IF
         CALL DATPUTC (NAME,'FILNAME', TMPFILE, 1)
         CALL DATPUTC (NAME,'FILACCESS', ACCESS, 1)
         CALL DATPUTC (NAME,'FILSYS', FILESYS, 1)
         CALL FTSOPEN (NAME)
         CALL FTSIMGWR (NAME)
         IF (TBLLIST.NE.' ') THEN
            CALL FTSTBLSW (NAME, TBLLIST)
         END IF
         CALL FTSCLOSE (NAME)
      ELSE IF (FILESYS.EQ.'SDE') THEN
         CALL DATPUTC (NAME,'FILNAME', FILENAME, 1)
         CALL DATPUTC (NAME,'FILACCESS', ACCESS, 1)
         CALL DATPUTC (NAME,'FILSYS', FILESYS, 1)
         CALL DATWRITE (NAME, FILENAME)
      ELSE IF (FILESYS.EQ.'PGM') THEN
         CALL FILPGMPU (NAME, FILENAME)
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
