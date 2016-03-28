C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftsclose.f	1.3    11/7/90
C
      SUBROUTINE FTSCLOSE (NAME)
C
CD Close FITS file on disk 
C
C
C 	NAME	CH*(*)	input	Name of directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSCLOSE')
C
C====================================================================
C
      IF (ERROR) GO TO 999
C
C Now Close file for access
C
      CALL FILSYSCL (NAME)
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
