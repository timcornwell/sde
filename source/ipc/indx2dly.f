C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)indx2dly.f	1.1    2/10/93
C
      SUBROUTINE INDX2DLY (INDEX, DELAY, NANT, NDELAYS)
C
CD Given an INDEX convert this into a delay set
C
C In IPCSSIM and IPCSHMM the set of model fringes is
C  indexed using an INDEX. To convert this to the delays corresponding
C  to that index this routine is used
C
C	INDEX	INTEGER         Input INDEX
C	DELAY	INTEGER(NANT)   Output Delay set
C	NANT	INTEGER         Number of holes in the apeture
C	NDELAYS	INTEGER         Number of delays per hole
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Apr 14 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           INDEX, NANT, NDELAYS, DELAY(NANT)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'INDX2DLY')
C
C Local Variables
C
      INTEGER I, REM
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Begin gode here
C
      REM = INDEX
      DO I = NANT, 1, -1
         DELAY(I) = MOD(REM, NDELAYS) + 1
         REM = REM/NDELAYS
      END DO
C
  999 CONTINUE
      END
