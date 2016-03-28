C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdqh3d.f	1.3    11/7/90
C
      SUBROUTINE GRDQH3D (VIS, WT, U, V, W, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, WSCALE, WOFFSET, UORIGIN, VORIGIN, WORIGIN,
     2   GVIS, PSF, NU, NV, NW, SHIFT, SUMWT)
C
CD Grid three dimensional complex data. There must be no points within the 
C support size of the edge of the grid. This version will properly grid only
C Hermitean data. The current timing is about 200 microsec per point to
C be gridded onto a 1024**2 grid on the CONVEX C-1.
C The scaling factors should be set so that USCALE*U + UOFFSET converts
C to grid cells centered at 0. This is then shifted to UORIGIN. Thus,
C for example, UOFFSET, VOFFSET should nearly always be zero, while
C UORIGIN = 1, VORIGIN = NV/2.
C
C     2   GVIS, PSF, NV, NV, NW, SUMWT), SHIFT)
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
C	WSCALE	REAL		input	Scaling factor to get to pixels
C	WOFFSET	REAL		input	Offset to get to pixels
C	UORIGIN	INT		input	Origin of u axis
C	VORIGIN	INT		input	Origin of v axis
C	WORIGIN	INT		input	Origin of w axis
C	GVIS	CMPLX(*)	output	Gridded data
C	PSF	LOG		input	TRUE for PSF
C	NU	INT		input	Size of gridded plane
C	NV	INT		input	Size of gridded plane
C	NW	INT		input	Size of gridded plane
C	SUMWT	REAL		output	Sum of weights
C Audit trail:
C	Added sum of weights
C				T.J.Cornwell	Jan 26 1989
C      Changed to Double for SHIFT
C				T.J.Cornwell	Jan 3 1990
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, NV, NW
      INTEGER	UORIGIN, VORIGIN, WORIGIN
      COMPLEX 	VIS(*), GVIS(NU, NV, NW)
      LOGICAL	PSF
      REAL	WT(*), SUMWT
      REAL	U(*), USCALE, UOFFSET
      REAL	V(*), VSCALE, VOFFSET
      REAL	W(*), WSCALE, WOFFSET
      DOUBLE PRECISION	SHIFT(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDQH3D')
C
      INTEGER 	IVIS, NGRID, USIGN
      INTEGER	UGRID, UCEN
      INTEGER	VGRID, VCEN
      INTEGER	WGRID, WCEN
      REAL	UCELL, VCELL, WCELL, UVWT, UWT, WWT, REVIS, IMVIS
      REAL	TWOPI, STOR, PHASE, LSUMWT
      COMPLEX	FVIS, ROTVIS
C==========================================================================
      IF (ERROR) GO TO 999
C
      TWOPI = 8 * ATAN(1.0)
      STOR = TWOPI / (360.0 * 3600.0)
C
      DO 7 WGRID = 1, NW
         DO 6 VGRID = 1, NV
            DO 5 UGRID = 1, NU
               GVIS(UGRID,VGRID,WGRID) = 0.0
  5         CONTINUE
  6      CONTINUE
  7   CONTINUE
C
      NGRID = 0
      LSUMWT = 0.0
      SUMWT = 0.0
C
C Start of loop elements to be gridded
C
         SUMWT = SUMWT + WT(IVIS)
      DO 10 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 10
         NGRID = NGRID + 1
         UCELL = USCALE * U(IVIS) + UOFFSET 
         VCELL = VSCALE * V(IVIS) + VOFFSET 
         WCELL = WSCALE * W(IVIS) + WOFFSET 
         PHASE = TWOPI * STOR * (U(IVIS) * SHIFT(1) + 
     1      V(IVIS) * SHIFT(2) + W(IVIS) * SHIFT(3))
         ROTVIS = VIS(IVIS) * CMPLX(COS(PHASE), -SIN(PHASE))
         IF (UCELL.GT.0.0) THEN
            FVIS = CONJG(ROTVIS)
            UCELL = UCELL + UORIGIN
            VCELL = VCELL + VORIGIN
            WCELL = WCELL + WORIGIN
         ELSE
            FVIS = ROTVIS
            UCELL = - UCELL + UORIGIN
            VCELL = - VCELL + VORIGIN
            WCELL = - WCELL + WORIGIN
         END IF
         IF (PSF) THEN
            REVIS = 1.0
            IMVIS = 0.0
         END IF
         UGRID = NINT(UCELL)
         VGRID = NINT(VCELL)
         WGRID = NINT(WCELL)
         GVIS(UGRID,VGRID,WGRID) = GVIS(UGRID,VGRID,WGRID) + 
     1      WT(IVIS) * FVIS
         LSUMWT = LSUMWT + WT(IVIS)
         IF (UGRID.EQ.UORIGIN) THEN
            VGRID = 2*VORIGIN-VGRID
            WGRID = 2*WORIGIN-WGRID
            GVIS(UGRID,VGRID,WGRID) = GVIS(UGRID,VGRID,WGRID) + 
     1         WT(IVIS) * CONJG(FVIS)
         END IF
C
  10  CONTINUE
C
      IF (LSUMWT.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No weight!')
         GO TO 999
      ELSE
         LSUMWT = (FLOAT(NU)-1)*FLOAT(NV)*FLOAT(NW)/LSUMWT
         DO 70 WGRID = 1, NW
            DO 60 VGRID = 1, NV
               DO 50 UGRID = 1, NU
               GVIS(UGRID, VGRID, WGRID) = LSUMWT * 
     1            GVIS(UGRID, VGRID, WGRID)
  50           CONTINUE
  60        CONTINUE
  70     CONTINUE
      END IF
C
 999  CONTINUE
      END
