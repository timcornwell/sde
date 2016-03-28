C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixgsfit.f	1.2    7/18/97
C
      SUBROUTINE PIXGSFIT (A, N1, N2, CELL, IPEAK, NROW, 
     $   AMIN, X1, X2, Y, SIG, BMAJ, BMIN, BPA, BZ, BAMP, BDX, BDY)
C
CD Fits a Gaussian to section of A centered on IPEAK
C  2-D Levenberg-Marquardt method
C
C	A	REAL(*)	input	array to fit
C	N1, N2	INT	input	dimensions of A
C	CELL	REAL(*)	input	cell size, degrees
C	IPEAK	INT(*)	input	position of peak of gaussian to fit
C	NROW	INT(*)	input	how far out from peak to fit
C	AMIN	INT	input	minimum (normed)beam amp for sampling ~.5
C	X1,X2	REAL(*)	input	Scratch arrays of size >= N1*N2
C	Y, SIG	REAL(*)	input	  "        "   "   "    "   "
C	BMAJ	REAL	in/out	Gaussian major axis, in degrees
C	BMIN	REAL	in/out	Gaussian minor axis, in degrees
C	BPA	REAL	in/out	Gaussian position angle
C	BZ	REAL	in/out	Z axis of 3-D Gaussian
C	BAMP	REAL	output	Value of the array at IPEAK
C	BDX	REAL	output	Offset from IPEAK(1) in pixels
C	BDY	REAL	output	Offset from IPEAK(2) in pixels
C
C Audit trail:
C	Cloned from PIXBMS1
C				D.S.Briggs	March 20 1994
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE='PIXGSFIT')
C
      INTEGER	N1, N2
      REAL	A(N1,N2), CELL(*), AMIN, X1(*), X2(*), Y(*), SIG(*),
     $   	BMAJ, BMIN, BPA, BDX, BDY, BZ, BAMP
      INTEGER	IPEAK(*), NROW(*)
C
      INTEGER	I, J, NPTS
      REAL	PARMS(7), COV(7,7), ALPHA(7,7), CHISQ, ALAMBDA,
     $   	LASTCHI, LASTLAST, XC, YC, AMIN2, T
C
      INTEGER	LISTA(7)
C
      EXTERNAL	GSFUNC
      DATA	LISTA/1,2,4,5,6,3,7/
C=======================================================================
      IF (ERROR) GO TO 999
C
C We sample the central part of A, 2*NROW(1)+1 by 2*NROW(2)+1
C
      BAMP = A(IPEAK(1), IPEAK(2))
      AMIN2 = ABS(AMIN * BAMP)
      NPTS = 0
      XC = 0.
      YC = 0.
      DO 60 J = MAX(1,IPEAK(2)-NROW(2)), MIN(N2, IPEAK(2)+NROW(2))
         DO 50 I = MAX(1,IPEAK(1)-NROW(1)), MIN(N1, IPEAK(1)+NROW(1))

C The sign on the RA can cause problems.  We just fit for what the beam
C "looks" like here, and worry about it later.
C
            IF (ABS(A(I,I)).GT.AMIN2) THEN
               NPTS = NPTS + 1
               X1(NPTS) = (I-IPEAK(1))*ABS(CELL(1))
               X2(NPTS) = (J-IPEAK(2))*ABS(CELL(2))
               Y(NPTS) = A(I,J)
               SIG(NPTS) = 1.0
            END IF
 50     CONTINUE
 60   CONTINUE
C
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1000) NPTS
 1000    FORMAT ('Used ',I3,' points for solution')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C We evalute this as a function of all the parameters, no matter which ones
C are allowed to vary.
C
      PARMS(1) = BMAJ
      PARMS(2) = BMIN
      PARMS(3) = BPA
      PARMS(4) = BAMP
      PARMS(5) = XC
      PARMS(6) = YC
      PARMS(7) = BZ
C					Initialization
      ALAMBDA = -.001
      CALL MRQMIN2 (X1, X2, Y, SIG, NPTS, PARMS, 7, LISTA, 6, COV,
     $   ALPHA, 7, CHISQ, GSFUNC, ALAMBDA)
      IF (SYSDEBUG) PRINT *, 'CHISQ = ', CHISQ
      IF (ERROR) GO TO 990
C
      LASTCHI = CHISQ
      LASTLAST = CHISQ
 100  CONTINUE
      CALL MRQMIN2 (X1, X2, Y, SIG, NPTS, PARMS, 7, LISTA, 6, COV,
     $   ALPHA, 7, CHISQ, GSFUNC, ALAMBDA)
      IF (SYSDEBUG) PRINT *, 'CHISQ = ', CHISQ
      IF (ERROR) GO TO 990
      IF ((ABS((CHISQ-LASTCHI)/LASTCHI).GT.1.E-3).OR.
     $    (ABS((LASTCHI-LASTLAST)/LASTLAST).GT.1.E-3)) THEN
         LASTLAST = LASTCHI
         LASTCHI = CHISQ
         GO TO 100
      END IF
C
      BMAJ = PARMS(1)
      BMIN = PARMS(2)
      BPA = PARMS(3)
      BAMP = PARMS(4)
      BDX = PARMS(5) / ABS(CELL(1))
      BDY = PARMS(6) / ABS(CELL(2))
      BZ = PARMS(7)
C
C					Just in case
C
      IF (BMIN.GT.BMAJ) THEN
         T = BMAJ
         BMAJ = BMIN
         BMIN = T
         BPA = BPA + 90.0
      END IF
C
C                                       Add map rotation.
C      BPA = BPA - MAPROT
      IF (BPA.GE.0.0) THEN
         BPA = MOD(BPA+90.0,180.0)-90.0
      ELSE
         BPA = MOD(BPA-90.0,180.0)+90.0
      END IF
      GO TO 999
C					Error return
C					The most likely scenario for a failure
C					is a beam that is undersampled
 900  CONTINUE
      CALL ERRCANCE
      CALL MSGPUT ('Solution for restoring beam failed','W')
      CALL MSGPUT ('Using default gaussian size of 1 pixel/beam','W')
      BMAJ = 1.0 * ABS(CELL(1))
      BMIN = 1.0 * ABS(CELL(2))
      BPA = 0.0
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

C
C Routine that actually does the function & gradient evaluation
C
      SUBROUTINE GSFUNC (X1, X2, PARMS, Y, DYDA, MA)
C
C The basic form of the fitting function is A * exp() + Bz
C
C The derivatives need be evaluated only for the parameters allowed to vary,
C but they're cheap enough to do anyway.
C
      INTEGER MA
      REAL X1, X2, Y, PARMS(MA), DYDA(MA)
C
      REAL	D2R
      PARAMETER	(D2R=3.14159265359/180.0)
C
      REAL	BMAJ, BMIN, BPA, BZ, BAMP, XC, YC
      REAL	DMAJ, DMIN, ST, CT, F, R2, B, DX, DY
C
      BMAJ = PARMS(1)
      BMIN = PARMS(2)
      BPA = PARMS(3)
      BAMP = PARMS(4)
      XC = PARMS(5)
      YC = PARMS(6)
      BZ = PARMS(7)
C
      DX = X1 - XC
      DY = X2 - YC
      ST = SIN(BPA*D2R)
      CT = COS(BPA*D2R)
      F = 8.0 * LOG(2.0)
      DMAJ = (-DX*ST + DY*CT)/BMAJ
      DMIN = (DX*CT + DY*ST)/BMIN
      R2 = F/2.*(DMAJ**2 + DMIN**2)
      B = BAMP * EXP(-R2)
      Y = B + BZ
      DYDA(1) = B * F * DMAJ**2 / BMAJ
      DYDA(2) = B * F * DMIN**2 / BMIN
      DYDA(3) = B * F * D2R * (DMAJ*(DX*CT + DY*ST)/BMAJ +
     $   DMIN*(DX*ST - DY*CT)/BMIN)
      DYDA(4) = EXP(-R2)
      DYDA(5) = B * F * (-ST*DMAJ/BMAJ + CT*DMIN/BMIN)
      DYDA(6) = B * F * (CT*DMAJ/BMAJ + ST*DMIN/BMIN)
      DYDA(7) = 1.0
      END

