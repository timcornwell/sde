C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixbmsh1.f	1.3    7/18/97
C
      SUBROUTINE PIXBMSH1 (A, N1, N2, CELL, IPEAK, NROW, 
     $   AMIN, X1, X2, Y, SIG, BMAJ, BMIN, BPA, BZ, BAMP)
C
CD Fits a Gaussian beam to section of A centered on IPEAK
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
C
C The selection code attempts to avoid including any sidelobes, even
C if they exceed the threshold, by starting from the center column and
C working out, exiting the loop when it crosses the threshold.  It
C assumes that the first time it finds a "good" point starting from
C the center and working out that it's in the main lobe.  Narrow,
C sharply ringing beams inclined at 45 degrees will confuse it, but that's
C even more pathological than most VLBI beams.
C
C Audit trail:
C	Structure of input cloned from pixbmshp 1.3, but uses different
C	method for the minimization.
C				D.S.Briggs	Aug 29 1993
C	Misunderstood the selection code, and didn't implement it quite
C	right.  This will now be more robust against nasty VLBI beams.
C				D.S.Briggs	Feb 8 1994
C	Added check for BMAJ >= BMIN, since the two can sometimes reverse
C	for nearly circular beams
C				D.S.Briggs	March 5 1994
C	Fixed some bugs in the derivative function.  These derivatives
C	have never been used, so it hasn't affected previous results,
C	but we might as well get them right.  We've gone to a SVD in
C	MRQMIN2, so we don't need to check for degeneracy here.
C				D.S.Briggs	March 21 1994
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE='PIXBMSH1')
C
      INTEGER	N1, N2
      REAL	A(N1,N2), CELL(*), AMIN, X1(*), X2(*), Y(*), SIG(*),
     $   	BMAJ, BMIN, BPA, BZ, BAMP
      INTEGER	IPEAK(*), NROW(*)
C
      INTEGER	I, J, NPTS, ILO, JLO, IFLIP, JFLIP, IROW, JROW
      REAL	PARMS(7), COV(7,7), ALPHA(7,7), CHISQ, ALAMBDA,
     $   	LASTCHI, LASTLAST, XC, YC, AMIN2, T
      LOGICAL	INLOBE
C
      INTEGER	LISTA(7)
C
      EXTERNAL	BS1FUNC
      DATA	LISTA/1,2,3,4,5,6,7/
C=======================================================================
      IF (ERROR) GO TO 999
C
C We sample the central part of A, 2*NROW(1)+1 by 2*NROW(2)+1
C
      BAMP = A(IPEAK(1), IPEAK(2))
      AMIN2 = AMIN * BAMP
      NPTS = 0
      XC = 0.
      YC = 0.
      IFLIP = 1
      JFLIP = 1
C                                       Loop through rows.
C					Include both above and below
C					in case we are fitting an image feature
      DO 75 JLO = 0, 1
         JFLIP = - JFLIP
C					Loop from 0 to NROW(2),
C					 then from 1 to NROW(2)
         DO 70 J = JLO, NROW(2)
C                                       The current row under consideration
            JROW = IPEAK(2) + J*JFLIP
C                                       Loop down row doing alternate
C                                       halves. Work our way out from
C					the center until we cross threshold
C					Don't include any sidelobes!
            DO 65 ILO = 0, 1
               IFLIP = - IFLIP
C					Start at center row
C					This may or may not be in the lobe,
C					if it's narrow and the PA is near
C					45 degrees
               INLOBE = A(IPEAK(1),JROW) .GT. AMIN2
               DO 60 I = ILO, NROW(1)
                  IROW = IPEAK(1) + I*IFLIP
C					Did we step out of the lobe?
                  IF (INLOBE .AND. (A(IROW,JROW) .LT. AMIN2))
     $               GO TO 65
C
                  IF (A(IROW,JROW) .GT. AMIN2 ) THEN 
                     INLOBE = .TRUE.
                     NPTS = NPTS + 1
C
C The sign on the RA can cause problems.  We just fit for what the beam
C "looks" like here, and worry about it later.
C
                     X1(NPTS) = (IROW-IPEAK(1))*ABS(CELL(1))
                     X2(NPTS) = (JROW-IPEAK(2))*ABS(CELL(2))
                     Y(NPTS) = A(IROW,JROW)
                     SIG(NPTS) = 1.0
                  END IF
 60            CONTINUE
 65         CONTINUE
 70      CONTINUE
 75   CONTINUE
C
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1000) NPTS
 1000    FORMAT ('Used ',I5,' points for solution')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Only the first three of these parameters are allowed to vary, but we
C evaluate it as a function of them all.
C
      PARMS(1) = BMAJ
      PARMS(2) = BMIN
      PARMS(3) = BPA
      PARMS(4) = BZ
      PARMS(5) = BAMP
      PARMS(6) = XC
      PARMS(7) = YC
C					Initialization
c      print *, 'A:',(parms(i), i=1,3)
      ALAMBDA = -.001
      CALL MRQMIN2 (X1, X2, Y, SIG, NPTS, PARMS, 7, LISTA, 3, COV,
     $   ALPHA, 7, CHISQ, BS1FUNC, ALAMBDA)
      IF (SYSDEBUG) PRINT *, 'CHISQ = ', CHISQ
c      print *, 'A:',(parms(i), i=1,3)
      IF (ERROR) GO TO 999
C
      LASTCHI = CHISQ
      LASTLAST = CHISQ
 100  CONTINUE
      CALL MRQMIN2 (X1, X2, Y, SIG, NPTS, PARMS, 7, LISTA, 3, COV,
     $   ALPHA, 7, CHISQ, BS1FUNC, ALAMBDA)
      IF (SYSDEBUG) PRINT *, 'CHISQ = ', CHISQ
c      print *, 'A:',(parms(i), i=1,3)
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
C
C					Just in case
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
      CALL MSGPUT ('Using default beam size of 1 pixel/beam','W')
      BMAJ = 1.0 * ABS(CELL(1))
      BMIN = 1.0 * ABS(CELL(2))
      BPA = 0.0
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END

C
C Routine that actually does the function & gradient evaluation
C
      SUBROUTINE BS1FUNC (X1, X2, PARMS, Y, DYDA, MA)
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
      BZ = PARMS(4)
      BAMP = PARMS(5)
      XC = PARMS(6)
      YC = PARMS(7)
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
      DYDA(4) = 1.0
      DYDA(5) = EXP(-R2)
      DYDA(6) = B * F * (-ST*DMAJ/BMAJ + CT*DMIN/BMIN)
      DYDA(7) = B * F * (CT*DMAJ/BMAJ + ST*DMIN/BMIN)
      END
