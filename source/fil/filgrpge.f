C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filgrpge.f	1.5    11/8/90
C
      SUBROUTINE FILGRPGE (NAME, FILENAME, GRPLIST, TBLLIST, FILESYS)
C
CD Get groups file.
C
C
C	name		CH*(*)	input	NAME of file as specified to user
C	filename	CH*(*)	input	Name of file
C	grplist	CH*(*)	input	List of groups to load
C	tbllist	CH*(*)	input	List of tables to load
C       filesys CH*(*)  input   FTS or SDE
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C      Slightly Reorganized code
C                              R.G. Marson     Sep 21 1989
C	Filesys not declared correctly => strange bugs on sparc, 
C	crashed on Convex
C				T.J.Cornwell	Nov 8 1990
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, GRPLIST, TBLLIST, FILESYS
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILGRPGE')
C
      CHARACTER		ACCESS*(SYSMXNAM)
C
      DATA		ACCESS / 'READ' /
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Now create the directory entry for this file and put 
C  some relevant info in it
C
      CALL DATCREAT (NAME)
      CALL DATPUTC (NAME,'FILNAME', FILENAME, 1)
      CALL DATPUTC (NAME,'FILACCESS', ACCESS, 1)
      CALL DATPUTC (NAME,'FILSYS', FILESYS, 1)
C
C Now open file and read in header
C
      IF (FILESYS.EQ.'FTS') THEN
         CALL HISOPEN (NAME)
         CALL FTSOPEN (NAME)
         CALL FTSGRPRE (NAME, GRPLIST)
         IF (TBLLIST.NE.' ') THEN
            CALL FTSTBLSR (NAME, TBLLIST)
         END IF
         CALL FTSCLOSE (NAME)
      ELSE IF (FILESYS.EQ.'SDE') THEN
         CALL DATREAD (NAME, FILENAME)
      ELSE
         MESSAGE = 'File system: '//FILESYS//' not supported'
         CALL ERRREPOR (ERRWRGTP, ROUTINE, MESSAGE)
         GOTO 999
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
