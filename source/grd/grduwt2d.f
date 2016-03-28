C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grduwt2d.f	1.10    11/1/94
C
      SUBROUTINE GRDUWT2D (WT, U, V, W, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, UORIGIN, VORIGIN, NEWWT, GWT, NU, NV, SHIFT)
C
CD 2-D uniform weighting. This version will properly reweight only
C Hermitean data. The current timing is about 70 microsec per point to
C be gridded onto a 1024**2 grid on the CONVEX C-1.
C The scaling factors should be set so that USCALE*U + UOFFSET converts
C to grid cells centered at 0. This is then shifted to UORIGIN. Thus,
C for example, UOFFSET, VOFFSET should nearly always be zero, while
C UORIGIN = 1, VORIGIN = NV/2.
C
C
C	WT	REAL(*)		input	Weights
C	U	REAL(*)		input	Coordinates of data
C	V	REAL(*)		input	Coordinates of data
C	W	REAL(*)		input	Coordinates of data
C	NVIS	INT		input	Number to be gridded
C	USCALE	REAL		input	Scaling factor to get to pixels
C	UOFFSET	REAL		input	Offset to get to pixels
C	VSCALE	REAL		input	Scaling factor to get to pixels
C	VOFFSET	REAL		input	Offset to get to pixels
C	UORIGIN	INT		input	Origin of u axis
C	VORIGIN	INT		input	Origin of v axis
C	NEWWT	REAL(*)		output	New weights
C	GWT	REAL(*)		output	Gridded weights
C	NU	INT		input	Size of gridded plane
C	NV	INT		input	Size of gridded plane
C	SHIFT	DBLE		input	Rotation matrix
C Audit trail:
C	Corrected weighting of points on V axis: now divide by two
C				T.J.Cornwell	Jan 8 1989
C	Added SHIFT
C				T.J.Cornwell	April 12 1991
C	Change to MAX (not MIN) in last step
C				T.J.Cornwell	May 2 1991
C	Finally fixed to do real uniform weighting. It was previously
C	ignoring the individual weights in the final stage.
C				T.J.Cornwell	May 4 1991
C	Warn user if some points not reweighted due to falling off the
C	grid.
C				D.S.Briggs	Oct 4 1994
C	Flag point off the grid, instead of leaving them alone.
C				D.S.Briggs	Nov 1 1994
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, SUPPU, OSAMPU, NV, SUPPV, OSAMPV
      INTEGER	UORIGIN, VORIGIN
      REAL 	GWT(NU, NV)
      REAL	WT(*), NEWWT(*)
      REAL	U(*), USCALE, UOFFSET
      REAL	V(*), VSCALE, VOFFSET
      REAL	W(*)
      DOUBLE  PRECISION	SHIFT (3,3)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDUWT2D')
C
      INTEGER 	IVIS, USIGN, NDROP
      INTEGER	UGRID, UGRIDI, UCEN
      INTEGER	VGRID, VGRIDI, VCEN
      REAL	UCELL, VCELL, UVWT, UWT, REVIS, IMVIS
      REAL	ULOCAL, VLOCAL
      COMPLEX	FVIS
C
      LOGICAL	DOSHIFT
C==========================================================================
      IF (ERROR) GO TO 999
C
      DO 6 VGRID = 1, NV
         DO 5 UGRID = 1, NU
            GWT (UGRID,VGRID) = 0.0
  5      CONTINUE
  6   CONTINUE
C
C Is there a shift?
C
      DOSHIFT = (SHIFT(1,1).NE.1.0D0).OR.(SHIFT(2,2).NE.1.0D0).OR.
     1   (SHIFT(3,3).NE.1.0D0)
C
C Start of loop elements to be gridded
C
      DO 10 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 10
         IF (DOSHIFT) THEN
            ULOCAL = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     1         SHIFT(3,1) * W(IVIS)
            VLOCAL = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     1         SHIFT(3,2) * W(IVIS)
            UCELL = USCALE * ULOCAL + UOFFSET 
            VCELL = VSCALE * VLOCAL + VOFFSET 
         ELSE
            UCELL = USCALE * U(IVIS) + UOFFSET 
            VCELL = VSCALE * V(IVIS) + VOFFSET 
         END IF
         IF (UCELL.GT.0.0) THEN
            UCELL = UCELL + UORIGIN
            VCELL = VCELL + VORIGIN
         ELSE
            UCELL = - UCELL + UORIGIN
            VCELL = - VCELL + VORIGIN
         END IF
         UCEN = NINT(UCELL) 
         VCEN = NINT(VCELL)
         IF ((UCEN.LT.1).OR.(UCEN.GT.NU).OR.(VCEN.LT.1).OR.
     $       (VCEN.GT.NV)) GO TO 10
         GWT (UCEN,VCEN) = GWT (UCEN,VCEN) + WT(IVIS)
 10   CONTINUE
C
      NDROP = 0
      DO 30 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 30
         IF (DOSHIFT) THEN
            ULOCAL = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     1         SHIFT(3,1) * W(IVIS)
            VLOCAL = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     1         SHIFT(3,2) * W(IVIS)
            UCELL = USCALE * ULOCAL + UOFFSET 
            VCELL = VSCALE * VLOCAL + VOFFSET 
         ELSE
            UCELL = USCALE * U(IVIS) + UOFFSET 
            VCELL = VSCALE * V(IVIS) + VOFFSET 
         END IF
         IF (UCELL.GT.0.0) THEN
            UCELL = UCELL + UORIGIN
            VCELL = VCELL + VORIGIN
         ELSE
            UCELL = - UCELL + UORIGIN
            VCELL = - VCELL + VORIGIN
         END IF
         UCEN = NINT(UCELL)
         VCEN = NINT(VCELL)
         IF ((UCEN.LT.1).OR.(UCEN.GT.NU).OR.(VCEN.LT.1).OR.
     $       (VCEN.GT.NV)) THEN
            NEWWT(IVIS) = -1.0
            NDROP = NDROP + 1
            GO TO 30
         END IF
         IF (GWT(UCEN, VCEN).GT.0.0) THEN
            IF(UCEN.EQ.UORIGIN) THEN
               IF(VCEN.EQ.VORIGIN) THEN
                  NEWWT(IVIS) = WT(IVIS) / GWT(UCEN, VCEN)
               ELSE 
                  NEWWT(IVIS) = 0.5 * WT(IVIS) / GWT(UCEN, VCEN)
               END IF
            ELSE
               NEWWT(IVIS) = WT(IVIS) / GWT(UCEN, VCEN)
            END IF
         ELSE
            NEWWT(IVIS) = -1.0
         END IF
C
  30   CONTINUE
C
       IF (NDROP.GT.0) THEN
 1000     FORMAT (I7,' points dropped off reweighting grid!')
          WRITE (MESSAGE,1000) NDROP
          CALL MSGPUT (MESSAGE,'W')
          CALL MSGPUT ('These points are now flagged bad','W')
       END IF
C
 999  CONTINUE
      END
