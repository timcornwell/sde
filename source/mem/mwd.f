C @(#)mwd.f	1.7	 10/30/92
C----------------------------------------------------------------------
C
C  Maximum Entropy Deconvolution Program
C
C     T.J. Cornwell
C     National Radio Astronomy Observatory
C     P.O. Box 0,
C     Socorro, New Mexico, 87801, USA.
C
C     tcornwel@nrao.edu
C     505 835 7333
C
C-----------------------------------------------------------------------
      SUBROUTINE MWDONEIT (DOINIT, DATASZ, IMSZ, NCONJ, FLUX, MEM, DEF,
     &   SIGMA, DOBS, DPRED, DZDLAM, DELLAM, PDZDLA, IOMEGA, TOL, 
     $   LENGTH, LAMBDA, MU, Z, FTRANS, LTRANS)
C
C Do one iteration
C
C Parameters:
C 
C  DOINIT       LOG   I   Initialise ? Should be true for first call
C  DATASZ       INT   I   Size of data set
C  IMSZ         INT   I   Size of image
C  NCONJ        INT   I   Number of successive conjugate steps
C  FLUX         REAL  I   Total flux (zero if not constrained)
C  MEM(*)       REAL  IO  Current MEM image
C  DEF(*)       REAL  I   Default image
C  SIGMA(*)     REAL  I   Array of expected errors
C  DOBS(*)      REAL  I   Array of observed data
C  DPRED(*)     REAL  I   Array of predicted data
C  IOMEGA       REAL  I   Expected Fit
C  TOL          REAL  I   Tolerance
C  LENGTH       REAL  O   Length of step
C  LAMBDA(*)    REAL  O   Lagrange multipliers of data
C  MU           REAL  O   Lagrange multiplier for fit OMEGA
C  Z            REAL  O   Potential function
C  DZDLAM, DELLAM, PDZDLA
C               REAL  I   Work space area for gradient arrays. Must be
C			  preserved between calls.
C  FTRANS       EXT   I   Name of routine for F->DPRED
C  LTRANS       EXT   I   Name of routine for LAMBDA->F
C
      LOGICAL DOINIT
      INTEGER DATASZ, IMSZ, NCONJ
      REAL FLUX, MEM(*), DEF(*), SIGMA(*), DOBS(*), DPRED(*), DZDLAM(*),
     &   DELLAM(*), PDZDLA(*), IOMEGA, TOL, LENGTH, LAMBDA(*), MU, Z
C
      INTEGER DCNTR
      EXTERNAL  FTRANS, LTRANS
      REAL GDS0, GDS1, STEP, STEPP, STEPM, SCALE, OMEGA
      REAL GRAD, PGRAD 
      SAVE GRAD, PGRAD
C
C Initialise. On the first step we just try for initial values
C of the LAMBDA's so we set OMEGA to zero just for this iteration.
C
      IF (DOINIT) THEN
         OMEGA = 0.0
         MU = 0.0
         DO 10 DCNTR = 1, DATASZ
            LAMBDA(DCNTR) = 0.0
            DZDLAM(DCNTR) = 0.0
            DELLAM(DCNTR) = 0.0
            PDZDLA(DCNTR) = 0.0
 10      CONTINUE
         CALL MWDCF (DATASZ, IMSZ, FLUX, LAMBDA, SIGMA, DOBS, MU,
     $      OMEGA, MEM, DEF, Z, LTRANS)
         CALL FTRANS (IMSZ, DATASZ, MEM, DPRED)
         CALL MWDCGRAD (DATASZ, LAMBDA, DZDLAM, PDZDLA, MU, SIGMA,
     $      DOBS, DPRED, GRAD, PGRAD)
      ELSE
         OMEGA = IOMEGA
      END IF
C
C Calculate delta lambda and the length of lambda.
C
      CALL MWDCDEL (DOINIT, DATASZ, NCONJ, LAMBDA, DZDLAM, DELLAM,
     $   GRAD, PGRAD)
      CALL MWDCLEN (DATASZ, DELLAM, LENGTH)
C
C Now set scaling according to initial length
C
      IF (DOINIT) THEN
         SCALE = 1.0/LENGTH
      ENDIF
      LENGTH = SCALE*LENGTH
C
C Find grad . step at start
C
      CALL MWDCGDST (DATASZ, LAMBDA, DELLAM, MU, SIGMA, DOBS, DPRED,
     $   GDS0)
C
C Ensure that we don't step too far
C
      IF (LENGTH.GT.0.0) THEN
         STEP = MIN(1.0, TOL/LENGTH)
         STEPM = MIN(10.0, MAX (1.0, TOL/LENGTH))
      ELSE
         STEP = 1.0
         STEPM = 10.0
      END IF
      STEPP = STEP
C
C Take step
C
      CALL MWDTAKST (DATASZ, LAMBDA, DELLAM, LAMBDA, SCALE*STEP)
C
C Calculate (Gradient.Step) at new position
C
      CALL MWDCF (DATASZ, IMSZ, FLUX, LAMBDA, SIGMA, DOBS, MU, OMEGA, 
     &   MEM, DEF, Z, LTRANS)
      CALL FTRANS (IMSZ, DATASZ, MEM, DPRED)
      CALL MWDCGDST (DATASZ, LAMBDA, DELLAM, MU, SIGMA, DOBS, DPRED,
     $   GDS1)
C
C Calculate optimum step using usual formula
C 
      IF (GDS0.NE.GDS1) THEN
         STEP = MAX(0.0, MIN(STEP*GDS0/(GDS0-GDS1), STEPM))
         IF (STEP.LE.0.0) THEN
            WRITE (*,*) 'MWDONEIT Error - Incorrect curvature'
            STEP = 1.0
         END IF
      ELSE
         WRITE (*,*) 'MWDONEIT Error - no progress'
         STEP = 1.0
      END IF
C
C Step to optimum place
C
      CALL MWDTAKST (DATASZ, LAMBDA, DELLAM, LAMBDA,
     $   SCALE*(STEP-STEPP))
C
C Now update MU
C
      IF (OMEGA.GT.0.0) THEN
         CALL MWDCMU  (DATASZ, OMEGA, SIGMA, LAMBDA, MU)
      END IF
C
C Calculate current image resulting from changes in lambda.
C
      CALL MWDCF (DATASZ, IMSZ, FLUX, LAMBDA, SIGMA, DOBS, MU, OMEGA, 
     &   MEM, DEF, Z, LTRANS)
      CALL FTRANS (IMSZ, DATASZ, MEM, DPRED)
C
      CALL MWDCGRAD (DATASZ, LAMBDA, DZDLAM, PDZDLA, MU, SIGMA,
     $   DOBS, DPRED, GRAD, PGRAD)
C
C Slowly update the scaling factor
C
      SCALE = SCALE * (3.0 + STEP)/4.0
C
      END
C
      SUBROUTINE MWDCGRAD (DATASZ, LAMBDA, DZDLAM, PDZDLA, MU, SIGMA,
     $   DOBS, DPRED, GRAD, PGRAD)
C
C Calculate the gradients of Z with respect to the unknowns
C
C Parameters:
C 
C  DATASZ       INT   I   Size of data set
C  LAMBDA(*)    REAL  I   Array of Lagrange multipliers
C  DZDLAM(*)    REAL  I   Gradients of Z w.r.t. Lambda
C  PDZDLAM(*)   REAL  IO  Previous gradients of Z w.r.t. Lambda
C  DELLAM(*)    REAL  IO  Actual step in Lambda
C  MU           REAL  I   Mu
C  SIGMA(*)     REAL  I   Array of expected errors
C  DOBS(*)      REAL  I   Array of observed data
C  DPRED(*)     REAL  I   Array of predicted data
C  GRAD         REAL  I   Current grad  . Current grad
C  PGRAD        REAL  I   Previous grad . Previous grad
C
      INTEGER DATASZ
      REAL LAMBDA(*), DZDLAM(*), PDZDLA(*), MU, SIGMA(*),
     $   DOBS(*), DPRED(*), GRAD, PGRAD
C
      INTEGER DCNTR
C
      PGRAD = GRAD
      DO 10 DCNTR = 1, DATASZ
         PDZDLA(DCNTR) = DZDLAM(DCNTR)
         DZDLAM(DCNTR) = DPRED(DCNTR) - DOBS(DCNTR)
 10   CONTINUE
      GRAD = 0.0
      IF(MU.GT.0.0) THEN
         DO 20 DCNTR = 1, DATASZ
            DZDLAM(DCNTR) = DZDLAM(DCNTR) 
     &         - LAMBDA(DCNTR)*SIGMA(DCNTR)**2/(2.0*MU) 
 20      CONTINUE
      END IF
      DO 40 DCNTR = 1, DATASZ
         GRAD = GRAD + DZDLAM(DCNTR)**2
 40   CONTINUE
C
      END
C
      SUBROUTINE MWDCGDST (DATASZ, LAMBDA, DELLAM, MU, SIGMA, 
     &   DOBS, DPRED, GDS)
C
C Calculate the dot product of the current step with the new gradient
C
C Parameters:
C 
C  DATASZ       INT   I   Size of data set
C  LAMBDA(*)    REAL  I   Array of Lagrange multipliers
C  DELLAM(*)    REAL  I   Actual step in Lambda
C  MU           REAL  I   Mu
C  SIGMA(*)     REAL  I   Array of expected errors
C  DOBS(*)      REAL  I   Array of observed data
C  DPRED(*)     REAL  I   Array of predicted data
C  GDS          REAL  O   Current gradient . step
C
      INTEGER DATASZ
      REAL LAMBDA(*), DELLAM(*), SIGMA(*), DOBS(*), DPRED(*)
      REAL MU, GDS
C
      INTEGER DCNTR
C
      GDS = 0.0
      DO 10 DCNTR = 1, DATASZ
         GDS = GDS + DELLAM(DCNTR) * (DPRED(DCNTR) - DOBS(DCNTR))
 10   CONTINUE
      IF (MU.GT.0.0) THEN
         DO 20 DCNTR = 1, DATASZ
            GDS = GDS - DELLAM(DCNTR) * LAMBDA(DCNTR) *
     $         SIGMA(DCNTR)**2/(2.0*MU)
 20      CONTINUE
      END IF
C
      END
C
      SUBROUTINE MWDCDEL (DOINIT, DATASZ, NCONJ, LAMBDA, DZDLAM, 
     &   DELLAM, GRAD, PGRAD)
C
C Calculate Conjugate gradient step
C
C Parameters:
C 
C  DATASZ       INT   I   Size of data set
C  NCONJ        INT   I   Number of successive conjugate steps
C  LAMBDA(*)    REAL  I   Array of Lagrange multipliers
C  DZDLAM(*)    REAL  I   Gradients of Z w.r.t. Lambda
C  DELLAM(*)    REAL  IO  Actual step in Lambda
C  GRAD         REAL  I   Current grad  . Current grad
C  PGRAD        REAL  I   Previous grad . Previous grad
C
      LOGICAL DOINIT
      INTEGER DATASZ, NCONJ
      REAL LAMBDA(*), DELLAM(*), DZDLAM(*)
      REAL GRAD, PGRAD
C
      REAL GAMMA
      INTEGER DCNTR, ICONJ
      SAVE ICONJ
C
      IF (DOINIT) THEN
         ICONJ = 0
         GAMMA = 0.0
      ELSE IF (ICONJ.GE.NCONJ) THEN
         ICONJ = 0
         GAMMA = 0.0
      ELSEIF (PGRAD.NE.0.0) THEN
         ICONJ = ICONJ + 1
         GAMMA = (GRAD / PGRAD)
         IF (ABS(GAMMA).GT.1000.0) THEN
            ICONJ = 0
            GAMMA = 0.0
         END IF
      ELSE
         ICONJ = 0
         GAMMA = 0.0
      END IF
C
      DO 10 DCNTR = 1, DATASZ
         DELLAM(DCNTR) = DZDLAM(DCNTR) + GAMMA * DELLAM(DCNTR)
  10  CONTINUE
C
      END
C
      SUBROUTINE MWDCLEN (DATASZ, DELLAM, LENGTH)
C
C Calculate length of step
C
C Parameters:
C 
C  DATASZ       INT   I   Size of data set
C  DELLAM(*)    REAL  I   Array of changes in Lagrange multipliers
C  LENGTH       REAL  O   Length
C
      INTEGER DATASZ
      REAL DELLAM(*), LENGTH
C
      INTEGER DCNTR
C
      LENGTH = 0.0
      DO 10 DCNTR = 1, DATASZ
         LENGTH = LENGTH + DELLAM(DCNTR)**2
 10   CONTINUE
      LENGTH = SQRT(LENGTH/DATASZ)
C
      END
C
      SUBROUTINE MWDCF (DATASZ, IMSZ, FLUX, LAMBDA, SIGMA, DOBS, MU,
     &   OMEGA, MEM, DEF, Z, LTRANS)
C
C Calculate F from transformed Lagrange multipliers
C
C Parameters:
C 
C  DATASZ       INT   I   Size of data set
C  IMSZ         INT   I   Size of image
C  FLUX         INT   I   Total flux of image
C  LAMBDA(*)    REAL  I   Array of Lagrange multipliers
C  SIGMA(*)     REAL  I   Array of expected errors
C  DOBS(*)      REAL  I   Observed data   
C  MU           REAL  I   Mu
C  OMEGA        REAL  I   Omega
C  MEM(*)       REAL  O   Current MEM image
C  DEF(*)       REAL  I   a prior image
C  Z            REAL  O   Potential
C  LTRANS       EXT   I   Name of routine for LAMBDA->F
C
      INTEGER DATASZ, IMSZ
      REAL LAMBDA(*), MEM(*), DEF(*), SIGMA(*), DOBS(*)
      REAL MU, OMEGA, Z, FLUX
      EXTERNAL LTRANS
C
      INTEGER IMCNTR, DCNTR
      REAL MAXDR, LAMTRM, NORM
      DATA MAXDR /1E20/
C
      LAMTRM = LOG(MAXDR)
C
      CALL LTRANS (DATASZ, IMSZ, LAMBDA, MEM)
C
      NORM = 0.0
      DO 10 IMCNTR = 1, IMSZ
         MEM(IMCNTR) = DEF(IMCNTR) * EXP(MIN(-MEM(IMCNTR),LAMTRM))
         NORM = NORM + MEM(IMCNTR)
  10  CONTINUE
C
      IF(FLUX.GT.0.0) THEN
         DO 20 IMCNTR = 1, IMSZ
            MEM(IMCNTR) = FLUX * MEM(IMCNTR) / NORM
  20     CONTINUE
         Z = -FLUX * LOG(NORM)
      ELSE
         Z = FLUX * NORM
      END IF
C
      DO 30 DCNTR = 1,DATASZ
         Z = Z - LAMBDA(DCNTR) * DOBS(DCNTR)
   30 CONTINUE
C
      IF (MU.GT.0.0) THEN
         Z = Z - MU * OMEGA 
         DO 40 DCNTR = 1,DATASZ
            Z = Z - (LAMBDA(DCNTR)*SIGMA(DCNTR))**2 / (4*MU)
   40    CONTINUE
      END IF
C
      END
C
      SUBROUTINE MWDTAKST (DATASZ, IN1, IN2, OUT, TSTEP)
C
C  Take recommended step
C
C Parameters:
C
C  DATASZ  INT  I  Size of data set
C  IN1(*)  REAL I  Array 1
C  IN2(*)  REAL I  Array 2
C  TSTEP   REAL I  Step
C  OUT(*)  REAL O  Output

      INTEGER DATASZ
      REAL IN1(*), IN2(*), OUT(*)
      INTEGER DCNTR
      REAL TSTEP
C
      DO 10 DCNTR = 1,DATASZ
        OUT(DCNTR) = IN1(DCNTR) + TSTEP*IN2(DCNTR)
  10  CONTINUE
C
      END
C
      SUBROUTINE MWDCMU (DATASZ, OMEGA, SIGMA, LAMBDA, MU)
C
C Calculate optimum value of MU for a given OMEGA
C
C Parameters:
C
C  DATASZ    INT  I   Size of data set
C  OMEGA    REAL I   Desired OMEGA
C  SIGMA(*)  REAL I   Array of expected errors
C  LAMBDA(*) REAL I   Array of Lagrange multipliers
C  MU        REAL O   Optimum Mu
C
      INTEGER DATASZ
      REAL OMEGA
      REAL SIGMA(*), LAMBDA(*), MU
C
      INTEGER DCNTR
      REAL DZDMU
C
      DZDMU = 0.0 
      DO 10 DCNTR = 1, DATASZ
         DZDMU = DZDMU + (LAMBDA(DCNTR)*SIGMA(DCNTR)) **2
 10   CONTINUE
C
      MU = SQRT(DZDMU/(4.0*OMEGA))
C
      END
