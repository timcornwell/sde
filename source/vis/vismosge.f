C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vismosge.f	1.2    11/7/90
C
      SUBROUTINE VISMOSGE (NAME, FILENAME)
C
CD Get mosaic visibility data from a file. 
C
C
C	NAME		CH*(*)	input	NAME of file as specified to user
C	FILENAME	CH*(*)	input	Name of file
C Audit trail:
C	New subroutine
C				T.J.Cornwell	Feb 3 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISMOSGE')
C
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Can only read SDE files
C
      CALL DATCREAT (NAME)
C
      CALL DATREAD (NAME, FILENAME)
      CALL DATCHKTP (NAME, 'VISMOSAIC')
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
