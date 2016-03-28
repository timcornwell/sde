C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filsysop.f	1.3    11/7/90
C
      SUBROUTINE FILSYSOP (NAME)
C
CD Open file 
C
C
C	NAME	CH*(*)	input	Name of directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILSYSOP')
C
      CHARACTER*(SYSMXNAM)	FILENAME, FILACCES, FILSTAT, 
     1                          FILEIN
      CHARACTER*1	IOMODE
      INTEGER		FILEID, DATFGETI, FILERR, L, STRLEN, NDUMMY
      LOGICAL		FILEXIST
C====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETC (NAME, 'FILNAME', FILEIN, 1, NDUMMY)
      CALL SYSTRANS (FILEIN, FILENAME)
      L = STRLEN (FILENAME)
C
      CALL DATGETC (NAME, 'FILACCESS', FILACCES, 1, NDUMMY)
      IF (ERROR) GO TO 990
      IF (FILACCES.EQ.'READ') THEN
         IF (.NOT.FILEXIST(FILEIN)) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 
     1         'Cannot find file '//FILENAME(:L))
            GO TO 999
         END IF
         IOMODE = 'r'
      ELSE IF (FILACCES.EQ.'WRITE') THEN
         IOMODE = 'w'
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Unknown access mode'//FILACCES)
         GO TO 999
      END IF
C
      IF (ERROR) GO TO 990
C
      FILEID = 0
      CALL FILCOPEN (FILENAME(:L), L, IOMODE, 1, FILEID)
      IF (FILEID.NE.0) THEN
         CALL DATPUTI(NAME, 'FILEID', FILEID, 1)
         GO TO 990
      END IF
C
C If we got here then there must have been an error opening the file
C
  100 CONTINUE
      MESSAGE = 'Cannot open '//FILENAME(:L)//' for '//FILACCES
      CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
      GO TO 999
  990 IF (ERROR) THEN
         CALL ERRTRACE(ROUTINE)
      END IF
C
  999 CONTINUE
      END
