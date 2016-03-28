C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcmkcm.f	1.1    2/10/93
C
      SUBROUTINE IPCMKCM (FRINGE, FWIDTH, FHEIGHT)
C
CD Given a fringe pattern turn this into a cumulative distribution
C
C This routine will given a MAPPIT fringe pattern (Fringes across the 
C verticle axis and wavelength across the horizontal axis) turn this into
C A cumulative distribution suitable use with a uniform random number generator
C This is done by forming a cumulative sum across each fringe and normalising
C
C	FRINGE	REAL(FWIDTH,FHEIGHT)	Output Fringe pattern
C	FWIDTH, FHEIGHT	INTEGER         Size of the above array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Sep 14 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           FWIDTH, FHEIGHT
      REAL              FRINGE(FWIDTH, FHEIGHT)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCMKCM')

C
C Function definitions
C

C
C Local Variables
C
      INTEGER           ROW, COL
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Do each wavelength individually
C
      DO COL = 1, FWIDTH
C
C form the cumulative sum along the columns
C
         DO ROW = 2, FHEIGHT
            FRINGE(COL, ROW) = FRINGE(COL, ROW) + FRINGE(COL, ROW - 1)
         END DO
C
C Normalise so that the maximum is one
C
         DO ROW = 1, FHEIGHT
            FRINGE(COL, ROW) = FRINGE(COL, ROW) / FRINGE(COL, FHEIGHT)
         END DO
      END DO
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
