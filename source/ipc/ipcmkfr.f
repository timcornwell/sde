C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcmkfr.f	1.3    6/9/93
C
      SUBROUTINE IPCMKFR (FRINGE, FSIZE1, FSIZE2, PFSIZE2, WAVE, 
     $     NANT, ANTY, ANTSIZE, 
     $     DELAYS, DSLOPES, SCALE,
     $     PUPIL,
     $     CF, CFSUPP, CFOSAMP, CCF, CCFSIZE)
C
CD Make an 'ideal' fringe pattern given the appropriate parameters
C
C This routine will given appropriate parameters of the physical
C situation generate the ideal fringe pattern that would be observed by
C the MAPPIT optical interferometer. This routine assumes the 
C object being observed is a point source with no wavelength dependent
C structure.
C
C	FRINGE	REAL(FSIZE1,FSIZE2)	Output Fringe pattern
C	FSIZE1,2 INTEGER        Size of the above array
C       WAVE    REAL(2)         Wavelength range of the above pattern
C       NANT    INTEGER         Number of holes in the mask
C       ANTY    REAL(NANT)      Positions of the holes in the mask (meters)
C       ANTSIZE REAL(NANT)      Size of the holes in the mask (meters)
C       DELAYS  REAL(NANT)      Atmosphere induced delays above each hole
C                               (meters)
C       DSLOPES REAL(NANT)      First Derivative of above slopes 
C                               (meters/meter)
C       SCALE   REAL            Scale size on the detector (radians/pixel)
C	PUPIL	COMPLEX(PFSIZE2,FSIZE1) Working array
C       CF     REAL((1+CFSUPP)*CFOSAMP*2+1) The oversampled convolving function
C       CFSUPP  INTEGER         The SUPPORT of the CF (in Pixels-typically 3)
C       CFOSAMP INTEGER         Oversampling of the CF (typically 128)
C       CCF     REAL(CCFSIZE)   The convolving correction function
C       CCFSIZE INTEGER         Size of the above array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Apr 14 1992
C       Use spheroidal functions for gridding and get rid of that linear
C       interpolation
C				R.G. Marson	May 14 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           FSIZE1, FSIZE2, PFSIZE2
      INTEGER           NANT, CFOSAMP, CFSUPP, CCFSIZE
      COMPLEX           PUPIL(PFSIZE2, FSIZE1)
      REAL              FRINGE(FSIZE1, FSIZE2)
      REAL              WAVE(2), CF(*), CCF(CCFSIZE)
      REAL              ANTY(NANT), ANTSIZE(NANT)
      REAL              DELAYS(NANT), DSLOPES(NANT)
      REAL              SCALE
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCMKFR')
      REAL              PI
      PARAMETER         (PI = 3.1415926535897932385)
      REAL              C
      PARAMETER         (C = 2.99792458E8)
      INTEGER           MAXNANT
      PARAMETER         (MAXNANT=100)
C
C Function definitions
C
      REAL              IPCWAVE
C
C Local Variables
C
      COMPLEX           DATA(MAXNANT)
      REAL              PHASE, LAMBDA, SUMWT, FCOL(MAXNANT), W(MAXNANT)
      INTEGER           ANT, PSIZE(2)
      INTEGER           ROW, COL, OFFSET, I
      INTEGER           NAXIS(SYSMXDIM), TADD
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Now Set up the pupil plane mask
C
      PSIZE(1) = PFSIZE2
      PSIZE(2) = FSIZE1
      DO ROW = 1, PSIZE(2)
         DO COL = 1, PSIZE(1)
            PUPIL(COL, ROW) = (0., 0.)
         END DO
      END DO
C
C Initialise the weights
C
      DO I = 1, NANT
         W(I) = 1.0
      END DO
C
C Do each wavelength seperately
C
      DO ROW = 1, PSIZE(2)
C
C Get the actual wavelength
C
         LAMBDA = IPCWAVE(ROW, PSIZE(2), WAVE)
C
C Put down a splat for each hole
C
         I = 1
         DO ANT = 1, NANT
C
C Calculate the phase of this splat
C
            PHASE = DELAYS(ANT) / LAMBDA * 2 * PI * C
C
C Calculate it position
C
            FCOL(I) = ANTY(ANT)*SCALE*FLOAT(PFSIZE2)/LAMBDA
C As ANTY(1) = 0 I'll move all the ant positions so tha ANTY(1) just fits in
C  (Considering the support it needs)
            FCOL(I) = FCOL(I) + CFSUPP
            IF ((FCOL(I).GE.0.).AND.
     $          (FCOL(I).LT.FLOAT(PSIZE(1)-CFSUPP))) THEN
C
C Cache this position/data for the gridding function
C
               DATA(I) = CMPLX(COS(PHASE),SIN(PHASE))
               I = I + 1
            ELSE
               WRITE (MESSAGE,
     $              '(A, I4, I4, F6.1, E10.2)') 'Out of Range ', 
     $              ROW, ANT, FCOL(I), LAMBDA*1E9
               CALL MSGPUT(MESSAGE, 'W')
            END IF
         END DO
C
C Here is where we call the gridding subroutine
C
         IF (I.GT.1) THEN
            CALL GRDH1D(DATA, W, FCOL, I-1, 1., 0., 1, PUPIL(1, ROW), 
     $           .FALSE., PSIZE(1), CF, CFSUPP, CFOSAMP, SUMWT)
         END IF
      END DO
C
C Now Fourier Transform this to give the amplitude on the detector
C
C#      CALL ARRX2AP('MODEL/PUPIL', 'AMP', 'PHAS')
C#      CALL FILIMGPU('AMP', 'T0/pupila.fit', ' ')
C#      CALL FILIMGPU('PHAS', 'T0/pupilp.fit', ' ')
C#      CALL DATDELAR('AMP')
C#      CALL DATDELAR('PHAS')
      CALL FFT12XX(PUPIL, PUPIL, -1, PSIZE(1), PSIZE(2), 1)
C
C Now apply the gridding correction factor
C
      DO ROW = 1, PSIZE(2)
         CALL PIXXCOR1(PUPIL(1, ROW), PSIZE(1), CCF, CCFSIZE, 
     $        'CORRECT', PUPIL(1, ROW), FLOAT(PSIZE(1)/2 + 1))
      END DO
C
C Now Multiply this by its conjugate to get the intensity distribution
C  Here is where an implicit transpose is done!
C
      OFFSET = (PSIZE(1)-FSIZE2)/2
      DO COL = 1, FSIZE1
         DO ROW = 1, FSIZE2
            FRINGE(COL, ROW) = PUPIL(ROW+OFFSET, COL) * 
     $           CONJG(PUPIL(ROW+OFFSET, COL))
         END DO
      END DO
C#      NAXIS(1) = FSIZE1
C#      NAXIS(2) = FSIZE2/2 + 1
C#      CALL DATMKGAR('TEMP', 2, NAXIS, 'X', TADD)
C#      CALL FFT12RX(FRINGE, MEMX(TADD), FSIZE1, FSIZE2, 2)
C#      CALL ARRX2AP('TEMP', 'REAL', 'IMAG')
C#      CALL FILIMGPU('REAL', 'T0/sfreqa.fit', ' ')
C#      CALL FILIMGPU('IMAG', 'T0/sfreqp.fit', ' ')
C#      CALL DATDELAR('REAL')
C#      CALL DATDELAR('IMAG')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C#      CALL SLEEP(5)
C
  999 CONTINUE
      END
