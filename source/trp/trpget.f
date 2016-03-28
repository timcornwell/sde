C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trpget.f	1.3	 7/20/92
C
      SUBROUTINE TRPGET (NAME, FILENAME, STOKES, GRPLIST, TBLLIST)
C
C Get triple product data from a file. 
C
C
C	NAME		CH*(*)	input	NAME of file as specified to user
C	FILENAME	CH*(*)	input	Name of file
C	STOKES		CH*(*)	input	List of Stokes parameters to load
C	GRPLIST	CH*(*)	input	List of groups to load
C	TBLLIST	CH*(*)	input	List of tables to load
C Audit trail:
C      New task
C				T.J.Cornwell	March 20 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, STOKES, GRPLIST, TBLLIST
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPGET')
C
C==================================================================
C
      IF (ERROR) GO TO 999
C
C At the moment we can only read SDE files
C
      CALL DATCREAT(NAME)
      CALL DATREAD (NAME, FILENAME)
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
