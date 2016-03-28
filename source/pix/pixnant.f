C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixnant.f	1.2    4/7/91
C
      INTEGER FUNCTION PIXNANT (BASELINE, NVIS)
C
CD Given the Baseline numbers extract the number of antennas
C
C The Baseline Array describes which antennas attach to a given visibility
C The coding scheme is Baseline number = (256 * ANT1 + ANT2)
C This limits the total number of antennas to less than 256
C
C       NVIS    INTEGER input   Size of BASELINE array
C	BASELINE REAL	input	Baseline array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Nov 12 1990
C	Initialized PIXNANT
C				T.J. Cornwell	April 7 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input declarations
C
      INTEGER           NVIS
      REAL              BASELINE(NVIS)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXNANT')
C
C Local Variables
C
      REAL                 ANT(256)
      INTEGER              I, ANT1, ANT2, NANT
C
C=======================================================================
      PIXNANT = 0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Initialise ANT array
C
      DO 10 I = 1, 256
         ANT(I) = 0
 10   CONTINUE
C
C Sort the antennas
C
      DO 20 I = 1, NVIS
         ANT1 = INT(BASELINE(I)/256)
         ANT2 = INT(BASELINE(I) - 256 * ANT1)
         IF ((ANT1.GT.0).AND.(ANT1.LT.256)) ANT(ANT1) = 1
         IF ((ANT2.GT.0).AND.(ANT2.LT.256)) ANT(ANT2) = 1
 20   CONTINUE
C
C Collate the total number of antennas
C
      NANT = 0
      DO 30 I = 1,256
         NANT = ANT(I) + NANT
 30   CONTINUE
      PIXNANT = NANT
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
