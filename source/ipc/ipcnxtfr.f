C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcnxtfr.f	1.1    2/10/93
C
      SUBROUTINE IPCNXTFR(DELAYS, NANT, NUMDLY, INDEX, 
     $     TZERO, RZERO, SEED)
C
CD Compute the next set of delays assuming a model of the atmosphere
C
C This routine will given the current delays (normalized) compute
C the next set of delays of a set of holes. It assumes a model of the
C atmosphere defined by tzero and rzero.
C Currently rzero is not used 
C
C	DELAYS	INTEGER(NANT)	Current delays->Next delays
C	NANT	INTEGER         Size of the above array
C       NUMDLY  INTEGER         Number of possible delays
C       INDEX   INTEGER         INDEX into fringe pattern for this delay set
C       RZERO,TZERO REAL        Fried's parameters (Normalized)
C       SEED    INTEGER         Random number sed
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Sep 18 1992
C	Use a gaussian model of the time delays
C				R.G. Marson	Nov 18 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           NANT, NUMDLY, INDEX, SEED, DELAYS(NANT)
      REAL              RZERO, TZERO
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCNXTFR')
C
C Function definitions
C
      INTEGER           DLY2INDX
C
C Local Variables
C
      REAL              SHIFT
      INTEGER           ANT
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Try for each antenna
C
      INDEX = 0
      DO ANT = 1, NANT
C#C
C#C Get random Number (between 1 and 0)
C#C
C#         CALL UTLRAND(SHIFT, SEED)
C#C
C#C If SHIFT < TZERO then set next delay to be one less than the current
C#C
C#         IF (SHIFT.LT.TZERO) THEN
C#            DELAYS(ANT) = DELAYS(ANT) - 1
C#C
C#C If SHIFT > 1-TZERO then set next delay to be one more than current
C#C
C#         ELSE IF ((1.-SHIFT).LT.TZERO) THEN 
C#            DELAYS(ANT) = DELAYS(ANT) + 1
C#C
C#C Otherwise if TZERO < SHIFT < 1-TZERO Then leave this delay alone
C#C
C#         END IF
C#C
C#C Ensure the delays do not get out of range
C#C
C#         DELAYS(ANT) = MIN(MAX(DELAYS(ANT), 1), NUMDLY)
C
C Get Gaussian random Number (ie. mean=0, varience=1)
C
         CALL UTLGRAND(SHIFT, SEED)
C
C Scale by Tzero as UTLGRAND produces Gaussian Random numbers
C  with zero mean and varience of 1
C  TZERO corresponds to Sigma in the standard formulation of gaussian dist.
C
         SHIFT = SHIFT*TZERO
         DELAYS(ANT) = DELAYS(ANT) + NINT(SHIFT)
C
C Ensure the delays do not get out of range
C
         DELAYS(ANT) = MIN(MAX(DELAYS(ANT), 1), NUMDLY)
      END DO
C
C Compute the Index to the corresponding fringe pattern
C
      INDEX = DLY2INDX(DELAYS, NANT, NUMDLY)
 999  CONTINUE
      END
