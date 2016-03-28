C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)muw.f	1.4    5/8/94
C
      SUBROUTINE MUWCHALB (IMSZ, MUW, DEF, DEFLEV, GCH, WT, ALPHA, 
     &   BETA, Q, FLUX, TFLUX, CHISQ, TCHISQ, AFIT, TOL, NRMGRD, GDG)
C
CD Find changes in alpha and beta
C
C  IMSZ       INT    I    IMAGE SIZE
C  MUW(*)     REAL   I    CURRENT SOLUTION
C  DEF(*)     REAL   I    DEFAULT IMAGE
C  DEFLEV     REAL   I    CONSTANT DEFAULT LEVEL
C  GCH(*)     REAL   I    GRAD CHI-SQUARED
C  WT(*)      REAL   I    WEIGHTS
C  ALPHA      REAL   I    LAGRANGE MULTIPLIER FOR CHISQ
C  BETA       REAL   I    LAGRANGE MULTIPLIER FOR FLUX
C  Q          REAL   IO   DIAG. VALUE GRAD GRAD CHISQ
C  TOL        REAL   I    TOLERANCE FOR SOLUTIION
C  NRMGRD     REAL   O    NORMALISED GRADIENT
C  GDG(4,4)   REAL   O    GRADIENT DOT PRODUCTS
C  GDSTEP     REAL   O    CURRENT GRADIENT . STEP
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Missing GRAD(C) if default image used
C				T.J.Cornwell	Jan 31 1931
C
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER 	IMSZ
      REAL	MUW(*), DEF(*), DEFLEV, GCH(*), WT(*), ALPHA, BETA, Q
      REAL	TOL, TFLUX, FLUX, CHISQ, TCHISQ, NRMGRD, AFIT
      DOUBLE PRECISION	GDG(4,4)
C
      INTEGER H,C,F,J
      PARAMETER (H=1)
      PARAMETER (C=2)
      PARAMETER (F=3)
      PARAMETER (J=4)
C
      INTEGER 	IMCNTR
      INTEGER 	AXIS1,AXIS2
C
      REAL	RHESS, GRAD(J), GGC, LENGTH, X, TANH
C======================================================================
      TANH(X) = (1.0 - EXP(-2.0*X))/(1+EXP(-2.0*X))
      IF (ERROR) GO TO 999
C
C  INITIALISE
C
       DO 1 AXIS2 = H,J
       DO 2 AXIS1 = H,J
          GDG(AXIS1,AXIS2) = 0.0
   2   CONTINUE
   1   CONTINUE
       GGC = ALPHA*Q
C
C  NOW DO ENTIRE IMAGE
C
C
       IF (DEFLEV.GT.-1E20) THEN
          DO 10 IMCNTR = 1,IMSZ
            GRAD(H) = -TANH((MUW(IMCNTR)-DEFLEV)*SQRT(WT(IMCNTR))/AFIT)
            RHESS = 1.0 / ((1.0-GRAD(H))**2 *SQRT(WT(IMCNTR))/AFIT +
     $         WT(IMCNTR)*GGC)
            GRAD(C) = GCH(IMCNTR)
            GDG(H,H) = GDG(H,H)+ GRAD(H)*RHESS*GRAD(H)
            GDG(H,C) = GDG(H,C)+ GRAD(H)*RHESS*GRAD(C)
            GDG(H,F) = GDG(H,F)+ GRAD(H)*RHESS
            GDG(C,C) = GDG(C,C)+ GRAD(C)*RHESS*GRAD(C)
            GDG(C,F) = GDG(C,F)+ GRAD(C)*RHESS
            GDG(F,F) = GDG(F,F)+ RHESS
  10     CONTINUE
      ELSE
         DO 20 IMCNTR = 1,IMSZ
            GRAD(H) = -TANH((MUW(IMCNTR)-DEF(IMCNTR))*
     $         SQRT(WT(IMCNTR))/AFIT)
            RHESS = 1.0 / ((1.0-GRAD(H))**2 *SQRT(WT(IMCNTR))/AFIT +
     $         WT(IMCNTR)*GGC)
            GRAD(C) = GCH(IMCNTR)
            GDG(H,H) = GDG(H,H)+ GRAD(H)*RHESS*GRAD(H)
            GDG(H,C) = GDG(H,C)+ GRAD(H)*RHESS*GRAD(C)
            GDG(H,F) = GDG(H,F)+ GRAD(H)*RHESS
            GDG(C,C) = GDG(C,C)+ GRAD(C)*RHESS*GRAD(C)
            GDG(C,F) = GDG(C,F)+ GRAD(C)*RHESS
            GDG(F,F) = GDG(F,F)+ RHESS
  20     CONTINUE
      END IF
C
C EVALUATE THE GRADIENT, NORMALISED APPROPRIATELY. 
C
      GDG (H,J) = GDG(H,H) - ALPHA * GDG(H,C) - BETA * GDG (H,F)
      GDG (C,J) = GDG(H,C) - ALPHA * GDG(C,C) - BETA * GDG (C,F)
      GDG (F,J) = GDG(H,F) - ALPHA * GDG(C,F) - BETA * GDG (F,F)
      GDG (J,J) = GDG(H,H) + ALPHA**2 *GDG(C,C) + BETA**2 * GDG(F,F)
     1   + 2 * ALPHA * BETA * GDG (C,F) - 2 * ALPHA * GDG (H,C)
     2   - 2 * BETA * GDG (H,F)      
      LENGTH = GDG(H,H) + ALPHA**2 * GDG(C,C) + BETA**2 * GDG(F,F)
C
      IF ((ALPHA.EQ.0.0).AND.(BETA.EQ.0.0)) THEN
         LENGTH = GDG(F,F)
      END IF
      NRMGRD = GDG(J,J)/LENGTH
      IF (ALPHA.EQ.0.0) NRMGRD = 0.0
C
      IF (NRMGRD.LE.TOL) THEN
         CALL MEMUPDAT (GDG, TOL, TFLUX, TCHISQ, FLUX, 
     1      CHISQ, LENGTH, ALPHA, BETA)
      ELSE 
         CALL MEMINIAB (GDG, ALPHA, BETA, TFLUX)
      END IF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE MUWCLSTP(IMSZ, MUW, DEF, DEFLEV, STP, GCH, WT, ALPHA, 
     1   BETA, Q, AFIT, FLUX, IMMAX, IMMIN, NRMGRD, GDG, GDSTEP)
C
C CALCULATE THE NEXT CHANGE IN THE MUW IMAGE FROM THE USUAL
C NEWTON-RAPHSON FORMULA. THE INVERSE OF THE HESSIAN IS APPROXIMATED BY
C ONLY TAKING THE INVERSE OF THE DIAGONAL ELEMENTS
C
C  IMSZ       INT    I    IMAGE SIZE
C  MUW(*)     REAL   I    CURRENT SOLUTION
C  DEF(*)     REAL   I    DEFAULT IMAGE
C  DEFLEV     REAL   I    CONSTANT DEFAULT LEVEL
C  GCH(*)     REAL   I    RESIDUAL IMAGE
C  STP(*)     REAL   IO   STEP IMAGE
C  ALPHA      REAL   I    LAGRANGE MULTIPLIER FOR CHISQ
C  BETA       REAL   I    LAGRANGE MULTIPLIER FOR FLUX
C  FLUX       REAL   O    ACTUAL FLUX
C  Q          REAL   IO   DIAG. VALUE GRAD GRAD CHISQ
C  IMMAX      REAL   O    IMAGE MAX.
C  IMMIN      REAL   O    IMAGE MIN.
C  NRMGRD     REAL   O    NORMALISED GRADIENT
C  GDG(4,4)   DBLE   O    GRADIENT DOT PRODUCTS
C  GDSTEP     REAL   O    CURRENT GRADIENT . STEP
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER 	IMSZ
      REAL	MUW(*), DEF(*), DEFLEV, GCH(*), STP(*), WT(*), ALPHA,
     $   BETA
      REAL	IMMIN, IMMAX, NRMGRD, GDSTEP, FLUX, Q, AFIT
      DOUBLE PRECISION	GDG(4,4)
C
      INTEGER H,C,F,J
      PARAMETER (H=1)
      PARAMETER (C=2)
      PARAMETER (F=3)
      PARAMETER (J=4)
C
      INTEGER 	IMCNTR
      INTEGER 	AXIS1,AXIS2
C
      REAL	RHESS,GRAD(J),GGC, LENGTH, X, TANH
C======================================================================
      TANH(X) = (1.0 - EXP(-2.0*X))/(1+EXP(-2.0*X))
      IF (ERROR) GO TO 999
C
C  INITIALISE
C
       IMMAX = -1E20
       IMMIN =  1E20
       FLUX = 0.0
       DO 1 AXIS2 = H,J
       DO 2 AXIS1 = H,J
          GDG(AXIS1,AXIS2) = 0.0
   2   CONTINUE
   1   CONTINUE
       GGC = ALPHA*Q
C
C  NOW DO ENTIRE IMAGE
C
       IF (DEFLEV.GT.-1E20) THEN
          DO 10 IMCNTR = 1,IMSZ
            FLUX = FLUX + MUW(IMCNTR)
            IMMIN = MIN (IMMIN, MUW(IMCNTR))
            IMMAX = MAX (IMMAX, MUW(IMCNTR))
            GRAD(H) = -TANH((MUW(IMCNTR)-DEFLEV)*SQRT(WT(IMCNTR))/AFIT)
            RHESS = 1.0 / ((1.0-GRAD(H))**2 *SQRT(WT(IMCNTR))/AFIT +
     $         WT(IMCNTR)*GGC)
            GRAD(C) = GCH(IMCNTR)
            GRAD(J) = GRAD(H) - ALPHA*GRAD(C) - BETA
            STP (IMCNTR) = RHESS*GRAD(J)
            GDG(H,H) = GDG(H,H)+ GRAD(H)*RHESS*GRAD(H)
            GDG(H,C) = GDG(H,C)+ GRAD(H)*RHESS*GRAD(C)
            GDG(H,F) = GDG(H,F)+ GRAD(H)*RHESS
            GDG(C,C) = GDG(C,C)+ GRAD(C)*RHESS*GRAD(C)
            GDG(C,F) = GDG(C,F)+ GRAD(C)*RHESS
            GDG(F,F) = GDG(F,F)+ RHESS
  10     CONTINUE
      ELSE
         DO 20 IMCNTR = 1,IMSZ
            FLUX = FLUX + MUW(IMCNTR)
            IMMIN = MIN (IMMIN, MUW(IMCNTR))
            IMMAX = MAX (IMMAX, MUW(IMCNTR))
            GRAD(H) = -TANH((MUW(IMCNTR)-DEF(IMCNTR))*
     $         SQRT(WT(IMCNTR))/AFIT)
            RHESS = 1.0 / ((1.0-GRAD(H))**2 *SQRT(WT(IMCNTR))/AFIT +
     $         WT(IMCNTR)*GGC)
            GRAD(C) = GCH(IMCNTR)
            GRAD(J) = GRAD(H) - ALPHA*GRAD(C) - BETA
            STP (IMCNTR) = RHESS*GRAD(J)
            GDG(H,H) = GDG(H,H)+ GRAD(H)*RHESS*GRAD(H)
            GDG(H,C) = GDG(H,C)+ GRAD(H)*RHESS*GRAD(C)
            GDG(H,F) = GDG(H,F)+ GRAD(H)*RHESS
            GDG(C,C) = GDG(C,C)+ GRAD(C)*RHESS*GRAD(C)
            GDG(C,F) = GDG(C,F)+ GRAD(C)*RHESS
            GDG(F,F) = GDG(F,F)+ RHESS
  20     CONTINUE
      END IF
C
C EVALUATE THE GRADIENT, NORMALISED APPROPRIATELY. 
C
      GDG (H,J) = GDG(H,H) - ALPHA * GDG(H,C) - BETA * GDG (H,F)
      GDG (C,J) = GDG(H,C) - ALPHA * GDG(C,C) - BETA * GDG (C,F)
      GDG (F,J) = GDG(H,F) - ALPHA * GDG(C,F) - BETA * GDG (F,F)
      GDG (J,J) = GDG(H,H) + ALPHA**2 *GDG(C,C) + BETA**2 * GDG(F,F)
     1   + 2 * ALPHA * BETA * GDG (C,F) - 2 * ALPHA * GDG (H,C)
     2   - 2 * BETA * GDG (H,F)      
      GDSTEP = GDG(J,J)
      LENGTH = GDG(H,H) + ALPHA**2 * GDG(C,C) + BETA**2 * GDG(F,F)
      IF (LENGTH.LE.0.0) THEN
         LENGTH = GDG(F,F)
      END IF
      NRMGRD = GDG(J,J)/LENGTH
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE MUWGDS (IMSZ, MUW, DEF, DEFLEV, STP, GCH, WT, ALPHA, 
     1   BETA, Q, AFIT, GDSTEP)
C
C Evaluate the dot product of the current image and the step
C
C  IMSZ       INT    I    IMAGE SIZE
C  MUW(*)     REAL   I    CURRENT SOLUTION
C  DEF(*)     REAL   I    DEFAULT IMAGE
C  GCH(*)     REAL   I    GRAD CHI-SQUARED
C  STP(*)     REAL   I    STEP IMAGE
C  ALPHA      REAL   IO   LAGRANGE MULTIPLIER FOR CHISQ
C  BETA       REAL   IO   LAGRANGE MULTIPLIER FOR FLUX
C  Q          REAL   I    Number of points per beam
C  GDSTEP     REAL   O    CURRENT GRAD . STEP
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER 	IMSZ
      REAL	MUW(*), DEF(*), DEFLEV, GCH(*), STP(*), WT(*), ALPHA,
     $   BETA
      REAL	GDSTEP, Q, AFIT
C
      INTEGER 	IMCNTR
      REAL	X, TANH
C======================================================================
      TANH(X) = (1.0 - EXP(-2.0*X))/(1+EXP(-2.0*X))
      IF (ERROR) GO TO 999
C
C  INITIALISE
C
      GDSTEP = 0.0
C
C  NOW DO ENTIRE IMAGE
C
       IF (DEFLEV.GT.-1E20) THEN
          DO 10 IMCNTR = 1,IMSZ
            GDSTEP = GDSTEP + STP(IMCNTR) * 
     1        (-TANH((MUW(IMCNTR)-DEFLEV)*SQRT(WT(IMCNTR))/AFIT) 
     2         - ALPHA*GCH(IMCNTR) - BETA)
  10     CONTINUE
      ELSE
         DO 20 IMCNTR = 1,IMSZ
            GDSTEP = GDSTEP + STP(IMCNTR) * 
     1         (-TANH((MUW(IMCNTR)-DEF(IMCNTR))*SQRT(WT(IMCNTR))/AFIT)
     $         - ALPHA*GCH(IMCNTR) - BETA)
  20     CONTINUE
      END IF
C
 999  CONTINUE
      END
      SUBROUTINE MUWONEIT (DOINIT, MUW, DEF, DEFLEV, STP, GCH, WT, Q, 
     1   TFLUX, TCHISQ, TOL, FLUX, CHISQ, ENTRPY, NRMGRD, AFIT, CALGCH)
C
C Does one MUW iteration. In this version, the pixels must be contiguous
C in memory. The routine CALGCH is called to evaluate both chi-squared
C and the gradient of chi-squared with respect to the image. You should
C call this routine repeatedly until FLUX and TFLUX, CHISQ and TCHISQ
C agree and NRMGRD is reasonably small e.g. < 0.05 say. TOL can be
C about 0.3 for reasonable speed in most cases. For difficult cases set
C it to be much smaller. Q is an estimate of the diagonal element of 
C grad-grad chi-squared, and should be initially set to some reasonable
C value. IF DEFLEV is greater than zero then it is taken as a constant
C default level and the default image is ignored.
C     This routine performs the MUW algorithm of Cornwell and Evans
C with some modifications suggested by Bob Sault.
C
C       DOINIT	LOG	input	Initialize ?
C	MUW	CH*(*)	input	Name of MUW image
C	DEF	CH*(*)	input	Name of default image
C	DEFLEV	REAL	input	Constant default level
C	STP	CH*(*)	input	Name of current step image
C	GCH	CH*(*)	input	Name of Grad chi-squared image
C	WT	CH*(*)	input	Name of weight image
C	Q	REAL	i/o	Current estimate of diagonal
C	TFLUX	REAL	input	Required Total flux
C	TCHISQ	REAL	input	Required chi-squared
C	TOL	REAL	input	Tolerance for solution
C	FLUX	REAL	output	Actual current flux
C	CHISQ	REAL	output	Actual current chi-squared
C	ENTRPY	REAL	output	Entropy of image
C	NRMGRD	REAL	output	Normalized gradient
C	AFIT	REAL	input	Actual noise in Jy/pixel
C	CALGCH	EXT	input	Routine to evaluate chi-squared and 
C				the gradient:
C				CALL CALGCH (MUW, GCH, CHISQ)
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Do not recalculate estimates of alpha & beta on initialization,
C	if values are passed in from a save file.
C				D.S.Briggs	May 8 1994
C----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      LOGICAL 		DOINIT
      CHARACTER*(*)	MUW, DEF, STP, GCH, WT
      REAL		DEFLEV, Q, TFLUX, TCHISQ, TOL, FLUX, CHISQ
      REAL		ENTRPY, NRMGRD, AFIT
#if COMP_SUN
      INTEGER           CALGCH
#endif
      EXTERNAL		CALGCH
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MUWONEIT')
C
      INTEGER 		IAX, NAX, NAXIS(SYSMXDIM), IMSZ, NDUMMY
      INTEGER		MADD, DADD, SADD, WADD, GADD
      REAL		SCALE, SCALEM, EPS, PONE, PZERO,
     1			IMMAX, IMMIN
      REAL		ALPHA, BETA
      DOUBLE PRECISION	GDG(4,4)
      CHARACTER		TYPE*1
C======================================================================
      IF (ERROR) GO TO 999
C
C Find axis info
C
      CALL DATGETAR (MUW, NAX, NAXIS, TYPE, MADD)
      IF (DEFLEV.GT.-1E20) THEN
         DADD = 1
      ELSE
         CALL DATGETAR (DEF, NAX, NAXIS, TYPE, DADD)
      END IF
      CALL DATGETAR (STP, NAX, NAXIS, TYPE, SADD)
      CALL DATGETAR (GCH, NAX, NAXIS, TYPE, GADD)
      CALL DATGETAR (WT, NAX, NAXIS, TYPE, WADD)
      IF (ERROR) GO TO 990
      IMSZ = 1
      DO 10 IAX = 1, NAX
         IMSZ = IMSZ * MAX (1, NAXIS(IAX))
  10  CONTINUE
C
C INITIALISE IF NECESSARY
C
      CALL DATGETR (MUW, 'ALPHA', ALPHA, 1, NDUMMY)
      CALL DATGETR (MUW, 'BETA', BETA, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         ALPHA = 0.0
         BETA = 0.0
      END IF
C
C IF THIS THE FIRST CALL
C
      IF (DOINIT) THEN
         CALL CALGCH (MUW, GCH, CHISQ)
         IF (ERROR) GO TO 990
         IF ((ALPHA.EQ.0.0).AND.(BETA.EQ.0.0)) THEN
            CALL MUWCHALB (IMSZ, MEMR(MADD), MEMR(DADD), DEFLEV, 
     $         MEMR(GADD), MEMR(WADD), ALPHA, BETA, Q, FLUX, TFLUX,
     $         CHISQ, TCHISQ, AFIT, TOL, NRMGRD, GDG)
         END IF
      END IF
C
      CALL MUWCLSTP (IMSZ, MEMR(MADD), MEMR(DADD), DEFLEV, 
     1   MEMR(SADD), MEMR(GADD), MEMR(WADD), ALPHA, BETA, Q, AFIT, 
     1   FLUX, IMMAX, IMMIN, NRMGRD, GDG, PZERO)
C
C LIMIT THE STEP TO LESS THAN THE TOLERANCE
C
      SCALE = 1.0
      SCALEM = 1.0
      IF (NRMGRD.GT.0.0) SCALEM = TOL/NRMGRD
      SCALE = MIN(1.0, SCALEM)
C
C TAKE STEP
C
      CALL MEMTAKST (IMSZ, MEMR(MADD), MEMR(SADD), 1.0, SCALE,
     1   -1E20, MEMR(MADD))
C
C  NOW CALCULATE THE GRADIENT AFTER THE STEP TO SEE IF IT WAS A GOOD IDE
C  THE VALUE OF THE GRADIENT WILL BE USED TO REVISE THE STEP LENGTH
C  FIRST, CALCULATE THE RESIDUALS FOR THIS NEW IMAGE
C
      CALL CALGCH (MUW, GCH, CHISQ)
      IF (ERROR) GO TO 990
C
C  CALCULATE DOT PRODUCT OF GRADIENT AND STEP
C
       CALL MUWGDS  (IMSZ, MEMR(MADD), MEMR(DADD), DEFLEV, MEMR(SADD), 
     1   MEMR(GADD), MEMR(WADD), ALPHA, BETA, Q, AFIT, PONE)
C
C  THIS IS THE OPTIMUW STEP
C
       EPS = 1.0
       IF (PZERO.NE.PONE) EPS = PZERO/(PZERO-PONE)
       IF (SCALE.NE.0.0) EPS = MIN(EPS, SCALEM/SCALE)
       IF (EPS.LE.0.0) THEN
          EPS = 1.0
          CALL MSGPUT ('MUWONEIT: warning EPS = 0.0', 'W')
       END IF
C
C  NOW STEP TO THE OPTIMUW POINT, CLIPPING THE STEP TO PREVENT EXCESSIVE
C  CHANGE ON ANY ONE ITERATION
C
      IF (ABS(EPS-1.0).GT.TOL) THEN
         CALL MEMTAKST (IMSZ, MEMR(MADD), MEMR(SADD), 1.0, 
     1      SCALE*(EPS-1.0), -1E20, MEMR(MADD))
         CALL CALGCH (MUW, GCH, CHISQ)
         IF (ERROR) GO TO 990
      END IF
C
C  CALCULATE ENTROPY AND FLUX OF NEW IMAGE
C
      CALL MUMCALHF (IMSZ, MEMR(MADD), MEMR(DADD), DEFLEV, FLUX, 
     1   ENTRPY)
C
C RE-ADJUST THE ESTIMATE OF THE BEAM VOLUME.
C
      Q = Q*(1/MAX(0.5,MIN(2.0,EPS))+1.0)/2.0
C
C  CHANGE ALPHA AND BETA
C
      CALL MUWCHALB (IMSZ, MEMR(MADD), MEMR(DADD), DEFLEV, 
     1   MEMR(GADD), MEMR(WADD), ALPHA, BETA, Q, FLUX, TFLUX, CHISQ, 
     2   TCHISQ, AFIT, TOL, NRMGRD, GDG)
C
      CALL DATPUTR (MUW, 'ALPHA', ALPHA, 1)
      CALL DATPUTR (MUW, 'BETA', BETA, 1)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
