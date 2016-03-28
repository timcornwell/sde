C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcvit.f	1.1    2/10/93
C
      SUBROUTINE IPCVIT (FPROB, TRPROB, TRACEBCK, NFRINGES, 
     $     WORKING, A)
C
CD This routine Traces through IPCS data using the Vitterbi Algorithm
C
C More extensive description with details etc.
C
C	FPROB	REAL (NFRINGES) The Probability of the current Frame
C	TRPROB	REAL(NFRINGES)  Probability of the current track
C	TRACEBCK INTEGER(NFRINGES) Records where the curent state came from
C       NFRINGES INTEGER        Size of the above arrays
C	WORKING	REAL (NFRINGES) Working array
C       A       REAL(NFRINGES, NFRINGES)  Precomputed state transtion probs.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Oct 20 1992
C	Changed to using LOG(Prob) to avoid overflow problems
C				R.G. Marson	Oct 26 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           NFRINGES, TRACEBCK(NFRINGES)
      REAL              FPROB(NFRINGES), TRPROB(NFRINGES)
      REAL              WORKING(NFRINGES), A(NFRINGES, NFRINGES)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCVIT')
C
C Local Variables
C
      REAL     PROB
      INTEGER OLDST, CURST
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Loop thru all possible states
C
      DO CURST = 1, NFRINGES
         WORKING(CURST) = TRPROB(1) + FPROB(CURST) +  
     $        A(1, CURST)
         TRACEBCK(CURST) = 1
         DO OLDST = 2, NFRINGES
            PROB = TRPROB(OLDST) + FPROB(CURST) + 
     $                                       A(OLDST, CURST)
C#            WRITE(MESSAGE, '(2I3, 4F8.3)') CURST, OLDST, 
C#     $           TRPROB(OLDST), FPROB(CURST), 
C#     $           A(OLDST, CURST), PROB
C#            CALL MSGPUT(MESSAGE, 'I')
            IF (PROB.GT.WORKING(CURST)) THEN
               WORKING(CURST) = PROB
               TRACEBCK(CURST) = OLDST
            END IF
         END DO
      END DO
C
C Now update the track Probabilities
C
      DO CURST = 1, NFRINGES
         TRPROB(CURST) = WORKING(CURST)
      END DO
C
  999 CONTINUE
      END
