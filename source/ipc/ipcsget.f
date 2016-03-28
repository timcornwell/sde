C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcsget.f	1.5    1/28/93
C
      SUBROUTINE IPCSGET (NAME, FILENAME, GRPLIST, TBLLIST)
C
CD Get ipcs data from a file. 
C
C	name		CH*(*)	input   NAME of file as specified to user
C	filename	CH*(*)	input   Name of file
C	grplist         CH*(*)  input   List of groups to load 
C	tbllist         CH*(*)  input   List of tables to load
C Audit trail:
C      Cloned from visget                                      
C                              R.G. Marson     Dec 28 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, GRPLIST, TBLLIST
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSGET')
C
      CHARACTER         FILESYS*(SYSMXNAM)
      LOGICAL		STRMATCH
C
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Check for null file name
C
      IF (FILENAME.EQ.' ') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Null file name')
         GO TO 990
      END IF
C
C Find file system type (SDE or FTS)
C
      CALL FILSYSEX (FILENAME, FILESYS)
C
C If this is an SDE file then just get it
C
      IF (STRMATCH (FILESYS, 'SDE')) THEN
         CALL DATCREAT (NAME)
         CALL DATREAD (NAME, FILENAME)
C
C Check if it is a FITS file
C
      ELSE IF (STRMATCH(FILESYS, 'FTS')) THEN
C
C Get groups file
C
         CALL FILGRPGE (NAME, FILENAME, GRPLIST, TBLLIST, FILESYS)
C
         CALL DATSETTP (NAME, 'IPCS')
C
C Otherwise return an error
C
      ELSE
         MESSAGE = 'File system: '//FILESYS//' not supported'
         CALL ERRREPOR (ERRWRGTP, ROUTINE, MESSAGE)
      END IF
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
