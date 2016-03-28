C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filgrppu.f	1.8	 7/18/97
C
      SUBROUTINE FILGRPPU (NAME, FILENAME, GRPLIST, TBLLIST, FILESYS)
C
CD Put groups file.
C
C	NAME            CH*(*)	input	NAME of file as specified to user
C	FILENAME        CH*(*)	input	File name
C	GRPLIST         CH*(*)	input	List of groups to write
C	TBLLIST	        CH*(*)	input	List of tables to write
C      FILESYS         CH*(*)  input   'FTS' or 'SDE'
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C      Re-organized file and made it work
C                              R.G. Marson     Sep 22 1989
C      Removed checking of SDETYPE as this is done in visput
C                              R.G. Marson     Feb 1 1989
C	Fixed String concatenations
C				T.J.Cornwell	Jan 6 1994
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, GRPLIST, TBLLIST
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILGRPPU')
C
      CHARACTER		FILESYS*(SYSMXNAM),
     $			ACCESS*(SYSMXNAM)
      LOGICAL           DATEXIST
C
      DATA		ACCESS / 'WRITE' /
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Check for directory existance
C
      IF (.NOT.DATEXIST(NAME)) THEN
         MESSAGE =  'Non existant name: '
         CALL STRAPPEN (MESSAGE, NAME)
         CALL ERRREPOR (ERRNTFND, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Keywords needed by some of the subroutines
C
      CALL DATPUTC (NAME,'FILNAME', FILENAME, 1)
      CALL DATPUTC (NAME,'FILACCESS', ACCESS, 1)
      CALL DATPUTC (NAME,'FILSYS', FILESYS, 1)
C
C Do the hard work depending on the file system type
C
      IF (FILESYS.EQ.'FTS') THEN
         CALL FTSOPEN (NAME)
         CALL FTSGRPWR (NAME, GRPLIST)
         IF (TBLLIST.NE.' ') THEN
            CALL FTSTBLSW (NAME, TBLLIST)
         END IF
         CALL FTSCLOSE (NAME)
      ELSE IF (FILESYS.EQ.'SDE') THEN
         CALL DATWRITE (NAME, FILENAME)
      ELSE
         MESSAGE = 'File system: not supported'
         CALL ERRREPOR (ERRWRGTP, ROUTINE, MESSAGE)
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
