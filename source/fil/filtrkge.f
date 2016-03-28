C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filtrkge.f	1.1    1/31/93
C
      SUBROUTINE FILTRKGE (NAME, FILENAME, TBLLIST)
C
CD Get track file.
C
C
C	NAME	 CH*(*)	input	NAME of file as specified to user
C	FILENAME CH*(*)	input	Name of file
C	TBLLIST CH*(*)	input	List of tables to load '*' => all
C				' ' => none
C Audit trail:
C       ALmost an exact copy of filimgge.f
C                               R. G. Marson    Dec 11 1992
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, TBLLIST
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILTRKGE')
C
      CHARACTER		FILESYS*(SYSMXNAM),
     3			ACCESS*(SYSMXNAM)
C
      LOGICAL		DATEXIST
      DATA		ACCESS / 'READ' /
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
C Create directory
C
      IF (.NOT.DATEXIST(NAME)) THEN
         CALL DATCREAT (NAME)
      END IF
C
      CALL FILSYSEX (FILENAME, FILESYS)
C
C Now open file and read in header
C
      IF (FILESYS.EQ.'FTS') THEN
         CALL HISOPEN (NAME)
         CALL DATPUTC (NAME,'FILNAME', FILENAME, 1)
         CALL DATPUTC (NAME,'FILACCESS', ACCESS, 1)
         CALL DATPUTC (NAME,'FILSYS', FILESYS, 1)
         CALL FTSOPEN (NAME)
         CALL FTSIMGRE (NAME)
         IF (TBLLIST.NE.' ') THEN
            CALL FTSTBLSR (NAME, TBLLIST)
         END IF
         CALL FTSCLOSE (NAME)
         CALL DATSETTP (NAME, 'TRACK')
      ELSE IF (FILESYS.EQ.'SDE') THEN
         CALL DATREAD (NAME, FILENAME)
         CALL DATCHKTP (NAME, 'TRACK')
         CALL HISOPEN (NAME)
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
