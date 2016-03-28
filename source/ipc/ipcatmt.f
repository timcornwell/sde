C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcatmt.f	1.1    2/10/93
C
      SUBROUTINE IPCATMT (PROB, NUMDLY, NANT, TZERO, RZERO)
C
CD Initialise the probability of atmospheric state changes with Time
C
C More extensive description with details etc.
C
C	PROB	REAL(NDELAY**NANT, NDELAY**NANT) Output Probability
C	NANT	INTEGER         Number of Holes
C	NDELAY	INTEGER         Number of Delays
C	TZERO	REAL            Time Coherence (Normalised)
C	RZERO	REAL            Spatial Coherence (Normalised)
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
      INTEGER           NANT, NUMDLY
      REAL              PROB(NUMDLY**NANT, NUMDLY**NANT)
      REAL              TZERO, RZERO
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCATMT')
      INTEGER           MAXNANT
      PARAMETER         (MAXNANT=100)
C
C Local Variables
C
      INTEGER NFRINGES, CURST, OLDST, N
      INTEGER ODELAY(MAXNANT), CDELAY(MAXNANT)
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Convert the Varience (= sigma^2) to -1/(2 * sigma^2)
C
      TZERO = -1./(2. * TZERO)
C
C Pre loop definitions
C
      NFRINGES = NUMDLY**NANT
C
C Begin the main loops here 
C
      DO CURST = 1,  NFRINGES
         CALL INDX2DLY(CURST-1, CDELAY, NANT, NUMDLY)
         DO OLDST = 1, NFRINGES
            CALL INDX2DLY(OLDST-1, ODELAY, NANT, NUMDLY)
            PROB(OLDST, CURST) = 0.
            DO N = 1, NANT
               PROB(OLDST, CURST) = PROB(OLDST, CURST) + 
     $              TZERO * FLOAT((CDELAY(N)-ODELAY(N))**2)
            END DO
C#            PRINT *, CURST, OLDST, PROB(OLDST, CURST)
         END DO
      END DO
C
C Can jump to here if an error found
C
  999 CONTINUE
      END
