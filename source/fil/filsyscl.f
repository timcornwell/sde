C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filsyscl.f	1.3    11/7/90
C
      SUBROUTINE FILSYSCL (NAME)
C
CD Close file 
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
      PARAMETER		(ROUTINE = 'FILSYSCL')
C
      INTEGER		NDUMMY, FILEID
C====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETI (NAME, 'FILEID', FILEID, 1, NDUMMY)
      IF (ERROR) GO TO 100
      CALL FILCCLOS(FILEID)
      GO TO 999
C
C If we got here then there must have been an error opening the file
C
  100 CONTINUE
      MESSAGE = 'Cannot close '//NAME
      CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
      GO TO 999
C
 999  CONTINUE
      END
