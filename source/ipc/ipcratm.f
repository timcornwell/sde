C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcratm.f	1.2    6/7/93
C
      SUBROUTINE IPCRATM (XFRAME, NFREQ, HEIGHT, VLA, 
     $     TRACK, NANT, DLYSTP, 
     $     PAD, WAVE)
C
CD Applies the  atmospheric corrections on a frame by frame basis
C
C This routine will apply a phase to each baseline to "correct" for
C  the atmospheric phase.
C
C	XFRAME	COMPLEX(NFREQ, HEIGHT) FFT of curent Frame
C	NFREQ	INTEGER         Width of the above array
C	HEIGHT	INTEGER         Height of the above array
C	TRACK	REAL(NANT)      Delays on each hole
C	NANT	INTEGER         Size of the above array
C       DLYSTP  REAL            Unit of Track Array
C       PAD     REAL            How much the XFRAME array is padded by
C       WAVE    REAL(NFREQ)     wavelength for each column
C       VLA     REAL(NANT, NANT, NFREQ) The visibility lookup aray
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Dec 7 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           NFREQ, HEIGHT, NANT
      COMPLEX           XFRAME(NFREQ, HEIGHT)
      REAL              TRACK(NANT),  DLYSTP
      REAL              VLA(NANT, NANT, NFREQ), WAVE(NFREQ), PAD
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCRATM')
      REAL              PI
      PARAMETER         (PI = 3.1415926535897932385)
      REAL              C
      PARAMETER         (C = 2.99792458E8)
C
C Function definitions
C

C
C Local Variables
C
      REAL    PHASE, DELAY      
      INTEGER I, J, F, R
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C#      PRINT *, 'DLYSTP = ', DLYSTP
C
C Begin gode here
C
      DO F = 1, NFREQ
C#         PRINT *, 'FREQUENCY: ', F, WAVE(F)
         DO I = 1, NANT - 1
            DO J = I + 1, NANT
               DELAY = (TRACK(J) - TRACK(I)) * DLYSTP
               PHASE = - DELAY/WAVE(F) * 2 * PI * C
C#               IF (F-1.EQ.NFREQ/2) THEN
C#                  PRINT *, 'Ant #1 ', I, ' Ant #2 ', J, ' DELAY = ', 
C#     $                 DELAY, ' PHASE = ', PHASE *180./3.1415
C#     $                 , VLA(I, J, F)
C#     $                 ,DLYSTP, TRACK(I), TRACK(J)
C#               END IF
               DO R = INT(VLA(I, J, F)-PAD/2.)+1, 
     $                INT(VLA(I, J, F)+PAD/2. + 1.0)+1
C#                  PRINT *, F, 'R: ', R, VLA(I,J,F), PAD
                  XFRAME(F, R) = XFRAME(F, R) * 
     $                 CMPLX(COS(PHASE),SIN(PHASE))
C#                  XFRAME(F, R) = (10000.0,0.0)
               END DO
            END DO
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
