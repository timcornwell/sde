C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixbmshp.f	1.5    6/12/94
C
      SUBROUTINE PIXBMSHP (A, N1, N2, CELL, IPEAK, NROW, 
     $   AMIN, BMAJ, BMIN, BPA, BZ, BAMP)
C
CD Fits a Gaussian beam to section of A centered on IPEAK
C
C	A	REAL	input	array to fit
C	N1,N2	INT	input   Dimensions of A
C	CELL	REAL	input	cell size, degrees
C	IPEAK	INT	input	position of peak of gaussian to fit
C	NROW	INT	input	how far out from peak to fit
C	AMIN	INT	input	minimum (normed)beam amp for sampling ~.5
C	BMAJ	REAL	output	Gaussian major axis, in degrees
C	BMIN	REAL	output	Gaussian minor axis, in degrees
C	BPA	REAL	output	Gaussian position angle
C	BZ	REAL	output	Z axis of 3-D Gaussian
C	BAMP	REAL	output	Value of the array at IPEAK
C
C The guts of this routine have undergone a nearly complete rewrite.
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
C	Retrofitted the VTESS BMSHP routine to SDE
C	Only works for 2-D
C				M.A.Holdaway	Aug 29 1989
C	Rewrote the majority of the code, for both a bug fix & clarity
C	The alternate flipping of sides is said to produce a more stable
C	solution in the case of very ill formed beams, and I've kept it,
C	but it does seem to throw away a lot of data.
C				D.S.Briggs	Aug 31 1993
C	Calling conventioned tweaked a little, and selection code
C	rewritten for clarity.
C				D.S.Briggs	Feb 8 1994
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE='PIXBMSHP')
C
      INTEGER	N1, N2
      REAL	A(N1,N2), CELL(*), AMIN, BMAJ, BMIN, BPA, BZ, BAMP
      INTEGER	IPEAK(*), NROW(*)
C
      INTEGER   I, J, IFLIP, JFLIP, ILO, JLO
      INTEGER   IROW, JROW, NPOINTS
      REAL      PXX, PXY, PYY, DX, DY, XFACT, AMIN2
      REAL	D, XC, YC, QA, QB, QC
      REAL	X11, X12, X13, X22, X23, X33, Y1, Y2, Y3
      LOGICAL	INLOBE
C=======================================================================
      IF (ERROR) GO TO 999
C
C We sample the central part of A, 2*NROW(1)+1 by 2*NROW(2)+1
C
      BZ = 0.
      IFLIP = 1
      JFLIP = 1
      NPOINTS = 0
C					Scaling factor for precision reasons
      XFACT = ABS (CELL(1))
C                                       Zero work variables
      X11 = 0.0
      X12 = 0.0
      X13 = 0.0
      X22 = 0.0
      X23 = 0.0
      X33 = 0.0
      Y1 = 0.0
      Y2 = 0.0
      Y3 = 0.0
C					Normalization, just in case
      BAMP = A(IPEAK(1),IPEAK(2))
      AMIN2 = AMIN * BAMP
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
                     NPOINTS = NPOINTS + 1
C                                       Compute displacements from
C                                       center.
                     DX = I * JFLIP * CELL(1) / XFACT
                     DY = J * IFLIP * CELL(2) / XFACT
C                                       Compute partials WRT CC1,CC2,CC3
                     PXX = DX * DX
                     PYY = DY * DY
                     PXY = DX * DY
C                                       Sum partials into X matrix and
C                                       Y vector.
                     Y1 = Y1 - LOG (A(IROW,JROW)/BAMP) * PXX
                     Y2 = Y2 - LOG (A(IROW,JROW)/BAMP) * PXY
                     Y3 = Y3 - LOG (A(IROW,JROW)/BAMP) * PYY
C
                     X11 = X11 + PXX**2
                     X12 = X12 + PXX*PXY
                     X13 = X13 + PXX*PYY
                     X22 = X22 + PXY**2
                     X23 = X23 + PXY*PYY
                     X33 = X33 + PYY**2
                  END IF
 60            CONTINUE
 65         CONTINUE
 70      CONTINUE
 75   CONTINUE
C
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1000) NPOINTS
 1000    FORMAT ('Used ',I3,' points for solution')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C					Matrix inversion for symmetric 3x3
C					courtesy Mathmatica
C
      D = -(X13**2*X22) + 2*X12*X13*X23 - X11*X23**2 - X12**2*X33 +
     $    X11*X22*X33
      IF (D.NE.0.0) THEN
C
         QA = (-Y3*X13*X22 + Y3*X12*X23 + Y2*X13*X23 - Y1*X23**2 -
     $      Y2*X12*X33 + Y1*X22*X33) / D
C
         QB = (Y3*X12*X13 - Y2*X13**2 - Y3*X11*X23 + Y1*X13*X23 +
     $      Y2*X11*X33 - Y1*X12*X33) / D
C
         QC = (-Y3*X12**2 + Y2*X12*X13 + Y3*X11*X22 - Y1*X13*X22 -
     $      Y2*X11*X23 + Y1*X12*X23) / D
      ELSE
C					Position angle probably degenerate
C					Drop the cross term, and try again
         D = X11*X33-X13**2
         IF (D.NE.0.0) THEN
            QA = (Y1*X33 - Y3*X13) / D
            QB = 0.0
            QC = (Y3*X11 - Y1*X13) / D
         ELSE
C					Still degenerate -- give up
            GO TO 900
         END IF
      END IF
C					Convert to normal parameters
      CALL UTLQ2EL (QA, QB, QC, 0.0, 0.0, -LOG(2.0),
     $   XC, YC, BMAJ, BMIN, BPA)
      IF (ERROR) GO TO 900
C                                       Negative sign in RA flips this.
      BPA = -BPA
C					Convert from mathematical convention
      BPA = BPA + 90.0
C                                       Add map rotation.  (Do we need this?)
C      BPA = BPA - MAPROT
      IF (BPA.GT.90.0) BPA = BPA - 180.0
      IF (BPA.LT.-90.0) BPA = BPA + 180.0
C					Restore physical units
      BMAJ = 2.0 * BMAJ * XFACT
      BMIN = 2.0 * BMIN * XFACT
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
  999 CONTINUE
      END
