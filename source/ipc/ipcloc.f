C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcloc.f	1.1    2/10/93
C
      SUBROUTINE IPCLOC (FRINGE, FWIDTH, FHEIGHT, XLOC, YLOC, SEED)
C
CD Given a fringe pattern determine where a photon should land
C
C This routine will given a MAPPIT cumulative fringe pattern 
C (Fringes across the 
C vertical axis and wavelength across the horizontal axis) and the position
C of a photon on the x (wavelength) axis determine where that photon 
C lands on the y (fringe) axis
C
C	FRINGE	REAL(FWIDTH,FHEIGHT)	Output Fringe pattern
C	FWIDTH, FHEIGHT	INTEGER      Size of the above array
C       XLOC    INTEGER         X position of photon (input)
C       YLOC    INTEGER         Y position of photon (output)
C       SEED    INTEGER         Random number seed
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
      INTEGER           FWIDTH, FHEIGHT, XLOC, YLOC, SEED
      REAL              FRINGE(FWIDTH, FHEIGHT)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCLOC')

C
C Function definitions
C

C
C Local Variables
C
      REAL              RANDOM
      INTEGER           I
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Generate a uniformly distributed random number (0-1)
C
      CALL UTLRAND(RANDOM, SEED)
C
C Find where to put it
C
      DO I = 1, FHEIGHT
         IF (RANDOM.LT.FRINGE(XLOC,I)) THEN
            YLOC = I
            GOTO 990
         END IF
      END DO
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
