C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipctbck.f	1.1    2/10/93
C
      SUBROUTINE IPCTBCK (TRPROB, TRACE, TRACK, 
     $     NFRINGES, NFRAMES, NANT, NUMDLYS)
C
CD Trace Back through an array and extract the track
C
C More extensive description with details etc.
C
C	TRPROB	REAL(NFRINGES)   Final Probabilities for each state
C	TRACE	INTEGER(NFRINGES, NFRAMES) Array to trace back through
C	TRACK	REAL(NANT, NFRAMES) The recovered Track
C	NFRINGES INTEGER         Number of fringes
C       NFRAMES INTEGER          Number of frames
C       NANT    INTEGER          Number of Holes
C       NUMDLYS INTEGER          Number of Delays per hole
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Oct 26 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           NFRAMES, NFRINGES, NANT, NUMDLYS
      REAL              TRPROB(NFRINGES)
      REAL              TRACK(NANT, NFRAMES)
      INTEGER           TRACE(NFRINGES, NFRAMES)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCTBCK')
      INTEGER           MAXNANT
      PARAMETER         (MAXNANT=100)
C
C Function definitions
C

C
C Local Variables
C
      REAL             MAXPROB
      INTEGER          MAXINDX, I, OLDST, J, DELAYS(MAXNANT)
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get the Best final State
C
      MAXPROB = TRPROB(1)
      OLDST = 1
      DO I = 2, NFRINGES
         IF (TRPROB(I).GT.MAXPROB) THEN
            MAXPROB = TRPROB(I)
            OLDST = I
         END IF
      END DO
C
C Now Trace back from this state
C
      DO I = NFRAMES, 1, -1
         CALL INDX2DLY(OLDST-1, DELAYS, NANT, NUMDLYS)
         DO J = 1, NANT
            TRACK(J, I) = FLOAT(DELAYS(J))
         END DO
         OLDST = TRACE(OLDST, I)
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
