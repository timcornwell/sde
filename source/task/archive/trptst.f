C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to test the triple product routines
C
CS Arguments: CALL SDEMAIN
CA
CA Audit trail:
CA	Original version: Audit trail comments go on this line
CA	and successive lines
CA				T.J.Cornwell	Jan 5 1989
CE
C--------------------------------------------------------------------
C
#include 		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPTST')
C
      CHARACTER*(SYSMXNAM)	MODE
      INTEGER		NANT
      PARAMETER 	(NANT = 6)
      COMPLEX		VIS(NANT, NANT), TRPARRY(NANT, NANT, NANT)
      REAL		SWT(NANT, NANT), TRPWT(NANT, NANT, NANT)
      COMPLEX		SVIS(NANT,NANT)
      INTEGER		I, J, K, SEED, NDUMMY
      REAL		RAN, AERR, PERR, AMP, PHASE, SAMP, 
     1			SPHASE, RPHASE, GRAN, NOISE
      INTEGER		ITER, NITER
      COMPLEX		CERR, PHASOR, STRP
      REAL		ORES, NRES, SUMWT
C==================================================================
      CALL MSGWELCO ('I test TRP routines')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
      CALL USRGETC ('Mode', MODE, 1, NDUMMY)
      CALL USRGETR ('Amp', AERR, 1, NDUMMY)
      CALL USRGETR ('Phase', PERR, 1, NDUMMY)
      CALL USRGETR ('Noise', NOISE, 1, NDUMMY)
      CALL USRGETI ('Npass', NITER, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
      CALL MSGPUT (' ', 'I')
      WRITE (MESSAGE, *) 'Mean Signal      = 1 unit per baseline'
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, *) 'with the following variations:'
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, *) 'Amplitude        = ',AERR,
     1   ' (fraction) per baseline'
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, *) 'Phase            = ',PERR,
     1   ' degrees per baseline'
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, *) 'We also add receiver noise per baseline:'
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, *) 'Additive noise   = ',NOISE,' per baseline'
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, *) 'Mode = ', MODE
      CALL MSGPUT (MESSAGE, 'I')
C
      DO 16 I = 1, NANT
         DO 15 J = I+1, NANT
            AMP = 1.0 + (RAN(SEED)-0.5) * AERR
            PHASE = RAN(SEED) * PERR
            VIS (I,J) = AMP * CMPLX(COSD(PHASE), SIND(PHASE))
            VIS (J,I) = CONJG(VIS(I,J))
  15     CONTINUE
  16  CONTINUE
C   
      WRITE (MESSAGE, 1000) NITER
 1000 FORMAT ('Averaging ',I7,' frames')
      CALL MSGPUT (MESSAGE, 'I')
C
C Now loop over a large number of realizations adding complex noise
C per baseline
C
      IF (NOISE.EQ.0.0) THEN
         NITER = 1
         CALL MSGPUT ('No noise requested: will only use one pass', 'I')
      END IF
C
      DO 30 ITER = 1, NITER
C
         DO 22 I = 1, NANT
            DO 21 J = I+1, NANT
               SVIS (I,J) = VIS(I,J) + NOISE * CMPLX(GRAN(SEED),
     $            GRAN(SEED))
               SVIS (J,I) = CONJG(SVIS(I,J))
   21       CONTINUE
   22    CONTINUE
C
         DO 27 I = 1, NANT
            DO 26 J = I+1, NANT
               DO 25 K = J+1, NANT
                  TRPARRY(I,J,K) = TRPARRY(I,J,K) + 
     1               SVIS(I,J) * SVIS(J,K) * CONJG(SVIS(I, K)) /
     2               FLOAT(NITER)
                  TRPWT(I,J,K) = TRPWT(I,J,K) + 1.0 /FLOAT(NITER)
  25           CONTINUE
  26        CONTINUE
  27     CONTINUE
C
  30  CONTINUE
C
C Set up initial estimate
C
      IF(MODE.EQ.'AMPPHI') THEN
         DO 41 J = 1, NANT
            DO 40 I = 1, NANT
               SVIS(I,J) = 1.0
  40        CONTINUE
  41     CONTINUE
      ELSE
         DO 43 J = 1, NANT
            DO 42 I = 1, NANT
               SVIS(I,J) = ABS(VIS(I,J))
 42         CONTINUE
 43      CONTINUE
      END IF
C
      CALL MSGPUT(' ', 'I')
      CALL TRPSOLVE (NANT, NANT, TRPARRY, TRPWT, SVIS, SWT, MODE, ORES,
     $   NRES, SUMWT)
C
C Cancel any error message which will be due to incomplete convergence
C
      CALL ERRCANCE
C
C Write out results
C
      WRITE (MESSAGE, *) 'Residuals before, after = ',ORES, NRES
      CALL MSGPUT (MESSAGE, 'I')
      CALL MSGPUT(' ', 'I')
      CALL MSGPUT('Final triple products', 'I')
      CALL MSGPUT(' ', 'I')
      CALL MSGPUT ('                Original          Estimate', 'I')
C
      DO 52 I = 1, NANT
         DO 51 J = I+1, NANT
            DO 50 K = J+1, NANT
               STRP = VIS(I,J) * VIS(J,K) * CONJG(VIS(I,K))
               AMP = ABS(STRP)
               PHASE = ATAN2D(AIMAG(STRP), REAL(STRP))
               STRP = SVIS(I,J) * SVIS(J,K) * CONJG(SVIS(I,K))
               SAMP = ABS(STRP)
               SPHASE = ATAN2D(AIMAG(STRP),REAL(STRP))
               WRITE(MESSAGE, 1100) I, J, K, AMP, PHASE, SAMP, SPHASE
 1100          FORMAT (3(I2,1X),4(1X,F8.3))
               CALL MSGPUT (MESSAGE, 'I')
  50        CONTINUE
  51     CONTINUE
  52  CONTINUE
      CALL MSGPUT(' ', 'I')
      CALL MSGPUT('Final visibilities', 'I')
      CALL MSGPUT(' ', 'I')
      CALL MSGPUT ('             Original          Estimate', 'I')
C
      DO 62 I = 1, NANT
         DO 61 J = I+1, NANT
            AMP = ABS(VIS(I,J))
            PHASE = ATAN2D(AIMAG(VIS(I,J)), REAL(VIS(I,J)))
            SAMP = ABS(SVIS(I,J))
            SPHASE = ATAN2D(AIMAG(SVIS(I,J)),REAL(SVIS(I,J)))
            WRITE(MESSAGE, 1200) I, J, AMP, PHASE, SAMP, SPHASE
 1200       FORMAT (2(I2,1X),4(1X,F8.3))
            CALL MSGPUT (MESSAGE, 'I')
  61     CONTINUE
  62  CONTINUE
C
 999  CONTINUE
      END
      REAL FUNCTION GRAN(SEED)
      INTEGER I, SEED
      REAL X, RAN
      X = 0.0
      DO 10 I=1,12
         X = X + (RAN(SEED) - 0.5)
 10   CONTINUE
      GRAN = X
      END
