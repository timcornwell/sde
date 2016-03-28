C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdqh2d.f	1.3    11/7/90
C
      SUBROUTINE GRDQH2D (VIS, WT, U, V, W, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, UORIGIN, VORIGIN, GVIS, PSF, NU, NV, 
     2   SHIFT, SUMWT)
C
CD Grid two dimensional complex data. There must be no points within the 
C support size of the edge of the grid. This version will properly grid only
C Hermitean data. The current timing is about 200 microsec per point to
C be gridded onto a 1024**2 grid on the CONVEX C-1.
C The scaling factors should be set so that USCALE*U + UOFFSET converts
C to grid cells centered at 0. This is then shifted to UORIGIN. Thus,
C for example, UOFFSET, VOFFSET should nearly always be zero, while
C UORIGIN = 1, VORIGIN = NV/2.
C
C
C	VIS	CMPLX(*)	input	Non-gridded data
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
C	GVIS	CMPLX(*)	output	Gridded data
C	PSF	LOG		input	TRUE for PSF
C	NU	INT		input	Size of gridded plane
C	NV	INT		input	Size of gridded plane
C	SHIFT	REAL		input	Shift matrix
C	SUMWT	REAL		output	Sum of weights
C Audit trail:
C	Added sum of weights
C				T.J.Cornwell	Jan 26 1989
C	Added shift matrix
C				T.J.Cornwell	Feb 18 1989
C      Changed to Double for SHIFT
C				T.J.Cornwell	Jan 3 1990
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, NV
      INTEGER	UORIGIN, VORIGIN
      COMPLEX 	VIS(*), GVIS(NU, NV)
      LOGICAL	PSF
      REAL	WT(*), SUMWT
      DOUBLE PRECISION	SHIFT(3,*)
      REAL	U(*), USCALE, UOFFSET
      REAL	V(*), VSCALE, VOFFSET
      REAL	W(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDQH2D')
C
      INTEGER 	IVIS, NGRID
      INTEGER	UGRID, UZERO
      INTEGER	VGRID, VZERO, OFFV
      REAL	UCELL, VCELL, LSUMWT, UVWT
      REAL	ULOCAL, VLOCAL, WLOCAL, TWOPI, PHASE
      COMPLEX	FVIS
C==========================================================================
      IF (ERROR) GO TO 999
C
      TWOPI = 8 * ATAN(1.0)
C
      DO 6 VGRID = 1, NV
         DO 5 UGRID = 1, NU
            GVIS(UGRID,VGRID) = 0.0
  5      CONTINUE
  6   CONTINUE
C
      NGRID = 0
C
C Start of loop elements to be gridded
C
      LSUMWT = 0.0
      SUMWT = 0.0
      DO 10 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 10
         ULOCAL = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     1      SHIFT(3,1) * W(IVIS)
         VLOCAL = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     1      SHIFT(3,2) * W(IVIS)
         WLOCAL = SHIFT(1,3) * U(IVIS) + SHIFT(2,3) * V(IVIS) +
     1      SHIFT(3,3) * W(IVIS)
         PHASE = TWOPI * (WLOCAL - W(IVIS))
         FVIS = VIS(IVIS) * CMPLX(COS(PHASE), -SIN(PHASE))
         UCELL = USCALE * ULOCAL + UOFFSET 
         VCELL = VSCALE * VLOCAL + VOFFSET 
         SUMWT = SUMWT + WT(IVIS)
         IF (UCELL.GT.0.0) THEN
            FVIS = CONJG(FVIS)
            UCELL = UCELL + FLOAT(UORIGIN)
            VCELL = VCELL + FLOAT(VORIGIN)
         ELSE
            UCELL = - UCELL + FLOAT(UORIGIN)
            VCELL = - VCELL + FLOAT(VORIGIN)
         END IF
         IF (PSF) THEN
            FVIS = 1.0
         END IF
         UGRID = NINT(UCELL)+1
         VGRID = NINT(VCELL)
         IF (UGRID.LT.1) GO TO 10
         IF (UGRID.GT.NU) GO TO 10
         IF (VGRID.LT.1) GO TO 10
         IF (VGRID.GT.NV) GO TO 10
         NGRID = NGRID + 1
         UVWT = WT(IVIS)
         GVIS(UGRID,VGRID) = GVIS(UGRID,VGRID) + UVWT * FVIS
         LSUMWT = LSUMWT + UVWT
  10  CONTINUE
C
C Symmetrize
C
      UZERO = UORIGIN + 1
      VZERO = VORIGIN
      DO 80 OFFV = 0, NV/2-1
         GVIS(UZERO,VZERO+OFFV) = GVIS(UZERO,VZERO+OFFV) +
     1      CONJG(GVIS(UZERO,VZERO-OFFV))
         GVIS(UZERO,VZERO-OFFV) = 
     1      CONJG(GVIS(UZERO,VZERO+OFFV))
  80  CONTINUE
C
      IF (LSUMWT.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No weight!')
         GO TO 999
      ELSE
         LSUMWT = FLOAT(NU-1)*FLOAT(NV)/LSUMWT
         DO 60 VGRID = 1, NV
            DO 50 UGRID = 1, NU-1
               GVIS(UGRID, VGRID) = LSUMWT * GVIS(UGRID+1, VGRID)
  50        CONTINUE
            GVIS(NU,VGRID) = 0.0
  60     CONTINUE
      END IF
C
 999  CONTINUE
      END
