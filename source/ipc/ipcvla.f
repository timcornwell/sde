C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcvla.f	1.2    6/7/93
C
      SUBROUTINE IPCVLA (VLA, NANT, NFREQ, ANTLOC, ANTDIAM, 
     $     WAVE, NFR, SCALE, WARRAY)
C
CD Initialise the visibility lookup array.
C
C  The visibility lokup array is one that given ANT1, ANT2, and the FREQ
C  will tell you the position on the Fourier Transform of that detector
C  fringe pattern of that fringe peak.
C
C	VLA(NANT, NANT,NFREQ) REAL The Visibilty Lookup Array
C	NANT	INTEGER         Number of Holes
C	NFREQ	INTEGER         Number of Frequencies
C	ANTLOC(NANT) REAL       Where the Holes are
C       WAVE    REAL(2)         The range (in meters) of the 
C                               starting and stopping wavelengths
C       NFR     INTEGER         Number of pixels on the fringe axis
C       SCALE   REAL            Scale size on the detector (see arrmkfr)
C       WARRAY  REAL(NFREQ)     The wavelength at each column (calculated)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Dec 4 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           NANT, NFREQ, NFR
      REAL              VLA(NANT, NANT, NFREQ)
      REAL              ANTLOC(NANT), ANTDIAM(NANT)
      REAL              SCALE, WAVE(2), WARRAY(NFREQ)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCVLA')

C
C Function definitions
C
      REAL              IPCWAVE
C
C Local Variables
C
      INTEGER           F, I, J
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Begin gode here
C
      DO F = 1, NFREQ
         WARRAY(F) = IPCWAVE(F, NFREQ, WAVE)
         DO I = 1, NANT - 1
            DO J = I+1, NANT
               VLA(I, J, F) = ABS(ANTLOC(J) - ANTLOC(I)) * 
     $              SCALE * FLOAT(NFR)/WARRAY(F)
C#               IF (F.EQ.NFREQ/2) THEN
C#               WRITE(MESSAGE, '(A, F7.2, A, I1, A, I1, A, F7.2)') 
C#     $              'Freq: ', WARRAY(I), ' Ant #1: ', I,
C#     $              ' Ant #2: ', J, ' Position: ', VLA(I, J, F)
C#               CALL MSGPUT(MESSAGE, 'I')
C#               END IF
               VLA(J, I, F) = VLA(I, J, F)
            END DO
            VLA(I, I, F) = 0.
         END DO
         VLA(NANT, NANT, F) = 0.
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
