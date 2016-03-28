C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcnxtdl.f	1.1    2/10/93
C
      SUBROUTINE IPCNXTDL (INDEX, DELAYS, IDELAYS, 
     $     NANT, NUMDLY, MINDLY, MAXDLY)
C
CD Compute the next set of delays
C
C This routine will given a the current set of delays compute the 
C next set according to the rule: Increment the delay on the lowest hole
C unless this is greater than the maximum in which case set this hole to the 
C minimum and move onto the next one. 
C
C       INDEX   INTEGER         The delay set we are after
C	DELAYS	REAL            The Current delay (new delay on return)
C	IDELAYS	INTEGER         The delays in integer form
C	NANT	INTEGER         Size of the above array
C       NUMDLY  INTEGER         Number of Delays
C       MINDLY  INTEGER         Min Delay value
C       MAXDLY  INTEGER         Max Delay Value (Actually Max is a bit less)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Sep 14 1992
C      modified to use indx2dly
C                               R. G. Marson    Dec 16 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           NANT, NUMDLY, INDEX, IDELAYS(NANT)
      REAL              MINDLY, MAXDLY, DELAYS(NANT)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCNXTDL')
C
C Function definitions
C

C
C Local Variables
C
      REAL              SCALE
      INTEGER           ANT
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Call indx2dly 
C
      CALL INDX2DLY(INDEX, IDELAYS, NANT, NUMDLY)
C
C Now just scale the answer
C
      SCALE = (MAXDLY-MINDLY) / FLOAT(NUMDLY)
      DO ANT = 1, NANT
         DELAYS(ANT) = FLOAT(IDELAYS(ANT) - 1) * SCALE + MINDLY
      END DO
C
 999  CONTINUE
      END



