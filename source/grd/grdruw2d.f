C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdruw2d.f	1.3    11/1/94
C
      SUBROUTINE GRDRUW2D (WT, U, V, W, NVIS, USCALE, UOFFSET, 
     $   VSCALE, VOFFSET, UORIGIN, VORIGIN, NEWWT, GWT, NU, NV, SHIFT,
     $   RMODE, ROBUST, DS)
C
CD 2-D robust uniform weighting.
C
C This version will properly reweight only Hermitean data.  The scaling
C factors should be set so that USCALE*U + UOFFSET converts to grid
C cells centered at 0. This is then shifted to UORIGIN. Thus, for
C example, UOFFSET, VOFFSET should nearly always be zero, while UORIGIN
C = 1, VORIGIN = NV/2.
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
C	SHIFT	DBLE(3,3)	input	Rotation matrix
C	RMODE	CH*(*)		input	Robustness mode
C	ROBUST	REAL(*)		input	Robust flux threshold
C	DS	REAL		input	Delta S for unit weight
C
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
C	Cloned from GRDUWT2D, and modified for ROBUST weighting
C				D.S.Briggs	Aug 29 1993
C	Warn user if some points not reweighted due to falling off the
C	grid.
C				D.S.Briggs	Oct 4 1994
C	Added azimuthal robust weighting.  (Mode AZ-NORM)
C				D.S.Briggs	Oct 24 1994
C	Flag points off the grid, instead of leaving them alone.
C				D.S.Briggs	Nov 1 1994
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, NV
      INTEGER	UORIGIN, VORIGIN
      REAL 	GWT(NU, NV)
      REAL	WT(*), NEWWT(*)
      REAL	U(*), USCALE, UOFFSET
      REAL	V(*), VSCALE, VOFFSET
      REAL	W(*), ROBUST(*), DS
      DOUBLE  PRECISION	SHIFT (3,3)
      CHARACTER*(*)	RMODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDRUW2D')
C
      INTEGER 	IVIS, NSEL, NDROP
      INTEGER	UGRID, UCEN
      INTEGER	VGRID, VCEN
      REAL	UCELL, VCELL
      REAL	ULOCAL, VLOCAL, F2, D2
      REAL	PI, RPA, RBSCALE, ROB, T
      DOUBLE PRECISION	SUMLOCWT, SUMWT
      LOGICAL	DOAZ
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
C Initialize for azimuthal robustness  Note that the position angle shifts
C 90 degrees coming to the uv plane, and also that we are going from the
C 0 = north to the mathematical 0 = x axis convention.  The minus sign
C reflects the common convention of displaying astromical images with the
C coordinates increasing to the left.
C
      IF (RMODE(1:2).EQ.'AZ') THEN
         DOAZ = .TRUE.
         ROB = (ROBUST(1) + ROBUST(2))/2.
         RPA = -ROBUST(3) + 90.0 - 90.0
 7       CONTINUE
         IF (RPA.LT.-90.0) THEN
            RPA = RPA + 180.0
            GO TO 7
         END IF
 8       CONTINUE
         IF (RPA.GT.90.0) THEN
            RPA = RPA - 180.0
            GO TO 8
         END IF
         PI = 4. * ATAN(1.0)
         RPA = RPA * PI / 180.0
         RBSCALE = (ROBUST(1) - ROBUST(2))/(PI/2.)
      ELSE
         DOAZ = .FALSE.
         ROB = ROBUST(1)
      END IF
C
C Start of loop elements to be gridded
C
      SUMWT = 0.D0
      NSEL = 0
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
         NSEL = NSEL + 1
         SUMWT = SUMWT + WT(IVIS)
 10   CONTINUE
C
C We use the approximation that all statistical weights are equal to
C calculate the average summed weights (over visibilities, not bins!)
C This is simply to try an ensure that the normalization of the robustness
C parameter is similar to that of the ungridded case, but it doesn't have
C to be exact, since any given case will require some experimentation.
C
      IF ((RMODE.EQ.'NORM').OR.(RMODE.EQ.'AZ-NORM')) THEN
         SUMLOCWT = 0.D0
         DO 21 VGRID = 1, NV
            DO 20 UGRID = 1, NU
               IF (GWT(UGRID, VGRID).GT.0.0) THEN
                  SUMLOCWT = SUMLOCWT + GWT(UGRID,VGRID)**2
               END IF
 20         CONTINUE
 21      CONTINUE
         IF (SYSDEBUG) THEN
            WRITE (MESSAGE, 1020) SUMLOCWT / SUMWT
 1020       FORMAT ('Average Summed Local Weight is',G12.4)
            CALL MSGPUT (MESSAGE, 'I')
         END IF
C					Some compilers are brain damaged
         F2 = (5.0*10.0**(-ROB))**2 / (SUMLOCWT / SUMWT)
         D2 = 1.0
      ELSE IF (RMODE.EQ.'ABS') THEN
         F2 = ROB**2
         D2 = 2.0 * DS**2
      ELSE IF (RMODE.EQ.'NONE') THEN
         F2 = 1.0
         D2 = 0.0
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Unrecognized RMODE')
         GO TO 999
      END IF
C
      NDROP = 0
      DO 30 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 30
         IF (DOSHIFT) THEN
            ULOCAL = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     1         SHIFT(3,1) * W(IVIS)
            VLOCAL = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     1         SHIFT(3,2) * W(IVIS)
         ELSE
            ULOCAL = U(IVIS)
            VLOCAL = V(IVIS)
         END IF
         UCELL = USCALE * ULOCAL + UOFFSET 
         VCELL = VSCALE * VLOCAL + VOFFSET 
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
C
         IF (DOAZ) THEN
            T = ATAN2(VLOCAL, ULOCAL)
            T = MOD(T-RPA+2.5*PI, PI) - PI/2.
            ROB = ROBUST(1) - ABS(T) * RBSCALE
            F2 = (5.0*10.0**(-ROB))**2 / (SUMLOCWT / SUMWT)
         END IF
C
         IF (GWT(UCEN, VCEN).GT.0.0) THEN
            IF ((UCEN.EQ.UORIGIN).AND.(VCEN.NE.VORIGIN)) THEN
               NEWWT(IVIS) = 0.5*WT(IVIS) / (GWT(UCEN,VCEN) * F2 + D2)
            ELSE
               NEWWT(IVIS) = WT(IVIS) / (GWT(UCEN,VCEN) * F2 + D2)
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
