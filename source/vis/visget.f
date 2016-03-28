C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visget.f	1.4    3/19/91
C
      SUBROUTINE VISGET (NAME, FILENAME, STOKES, GRPLIST, TBLLIST)
C
CD Get visibility data from a file. 
C
C	NAME		CH*(*)	input	NAME of file as specified to user
C	FILENAME	CH*(*)	input	Name of file
C	STOKES		CH*(*)	input	List of Stokes parameters to load
C	GRPLIST	CH*(*)	input	List of groups to load
C	TBLLIST	CH*(*)	input	List of tables to load
C Audit trail:
C	Added SDETYPE specification
C				T.J.Cornwell	Feb 3 1989
C      Re-organized to work better with the FILGRPGE subroutine
C                              R.G. Marson     Sep 21 1989
C	VISGET had forgotten how to read SDE files
C				M.A.Holdaway	March 19 1991
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, STOKES, GRPLIST, TBLLIST
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISGET')
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
         GO TO 999
      END IF
C
C Find file system type (SDE or FTS)
C
      CALL FILSYSEX (FILENAME, FILESYS)
C
C If this is an SDE file then just get it
C
      IF (STRMATCH (FILESYS, 'SDE')) THEN
         CALL DATCREAT(NAME)
         CALL DATREAD (NAME, FILENAME)
         GO TO 999
C
C Check if it is a FITS file
C
      ELSE IF (STRMATCH(FILESYS, 'FTS')) THEN
C
C Get groups file
C
         CALL FILGRPGE (NAME, FILENAME, GRPLIST, TBLLIST, FILESYS)
C
C Now filter to get required STOKES
C
         CALL VISSTD (NAME, STOKES)
C
         CALL DATSETTP (NAME, 'VIS')
C
C Otherwise return an error
C
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

