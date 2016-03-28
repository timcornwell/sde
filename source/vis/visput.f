C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visput.f	1.4    11/7/90
C
      SUBROUTINE VISPUT (NAME, FILENAME, CLASS, STOKES, GRPLIST,
     $   TBLLIST)
C
CD Put visibility data to a file. 
C
C	name		CH*(*)	input	NAME of file as specified to user
C	filename	CH*(*)	input	Name of file
C	CLASS		CH*(*)	input	Name of class to save e.g. OBS
C	stokes		CH*(*)	input	List of stokes parameters to save
C	grplist		CH*(*)	input	List of groups to save
C      tbllist	CH*(*)	input	List of tables to save
C Audit trail:
C	New subroutine
C				T.J.Cornwell	Feb 3 1989
C      Added support for FITS files
C                              R.G. Marson     Sep 22 1989
C      changed CLASS input to stokes and added SDETYPE checking
C                              R.G Marson      Jan 20 1990
C	Added CLASS input
C				T.J.Cornwell	Nov 1 1990
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, STOKES, GRPLIST, TBLLIST,
     $			CLASS
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPUT')
C
      CHARACTER         FILESYS*(SYSMXNAM)
      LOGICAL           STRMATCH
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
C     Check that name is of the VIS type
C
      CALL DATCHKTP(NAME, 'VIS')
C
C Find file system type (SDE or FTS) by looking at filename
C  extension. (default = FTS)
C
      CALL FILSYSEX (FILENAME, FILESYS)
C
C If this is an SDE file then just write it
C
      IF (STRMATCH (FILESYS, 'SDE')) THEN
         CALL FILGRPPU (NAME, FILENAME, GRPLIST, TBLLIST, FILESYS)
      ELSE IF (STRMATCH (FILESYS, 'FTS')) THEN
C
C Put the groups file
C
         CALL VISDTS(NAME, CLASS, STOKES)
         CALL FILGRPPU (NAME, FILENAME, GRPLIST, TBLLIST, FILESYS)
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


