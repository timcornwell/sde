C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vismospu.f	1.2    11/7/90
C
      SUBROUTINE VISMOSPU (NAME, FILENAME)
C
CD Put mosaic visibility data to a file. 
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
      PARAMETER		(ROUTINE = 'VISMOSPU')
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
