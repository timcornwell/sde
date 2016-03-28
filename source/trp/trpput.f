C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trpput.f	1.4	 7/20/92
C
      SUBROUTINE TRPPUT (NAME, FILENAME, CLASS, STOKES, GRPLIST,
     $   TBLLIST)
C
C Put Triple product data to a file
C
C
C	NAME		CH*(*)	input	NAME of file as specified to user
C	FILENAME	CH*(*)	input	Name of file
C	CLASS		CH*(*)	input	List of CLASS parameters to save
C	GRPLIST		CH*(*)	input	List of groups to save
C	TBLLIST	CH*(*)	input	List of tables to save
C Audit trail:
C	New subroutine
C				T.J.Cornwell	March 15 1989
C	Added Stokes
C				T.J.Cornwell	Dec 19 1990
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, CLASS, GRPLIST, TBLLIST,
     $  STOKES
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPPUT')
C
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Can only write SDE files
C
      CALL DATWRITE (NAME, FILENAME)
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
