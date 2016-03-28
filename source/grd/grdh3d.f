C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdh3d.f	1.3    11/7/90
C
      SUBROUTINE GRDH3D (VIS, WT, U, V, W, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, WSCALE, WOFFSET, UORIGIN, VORIGIN, WORIGIN,
     2   GVIS, PSF, NU, NV, NW, GRIDFNU, SUPPU, OSAMPU,
     3   GRIDFNV, SUPPV, OSAMPV, GRIDFNW, SUPPW, OSAMPW, SHIFT, SUMWT)
C
CD Grid three dimensional complex data. There must be no points within the 
C support size of the edge of the grid. This version will properly grid only
C Hermitean data. The current timing is about 400 microsec per point to
C be gridded onto a 1024**2 grid on the CONVEX C-1.
C The scaling factors should be set so that USCALE*U + UOFFSET converts
C to grid cells centered at 0. This is then shifted to UORIGIN. Thus,
C for example, UOFFSET, VOFFSET should nearly always be zero, while
C UORIGIN = 1, VORIGIN = NV/2.
C
C     2   GVIS, PSF, NU, NV, NW, GRIDFNU, SUPPU, OSAMPU,
C     3   GRIDFNV, SUPPV, OSAMPV, GRIDFNW, SUPPW, OSAMPW, SHIFT, SUMWT)
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
C	GRIDFNU	REAL		input	Gridding function
C	SUPPU	INT		input	Support of gridding function
C	OSAMPU	INT		input	Over-sampling factor
C	GRIDFNV	REAL		input	Gridding function
C	SUPPV	INT		input	Support of gridding function
C	OSAMPV	INT		input	Over-sampling factor
C	GRIDFNW	REAL		input	Gridding function
C	SUPPW	INT		input	Support of gridding function
C	OSAMPW	INT		input	Over-sampling factor
C	SHIFT	REAL		input	Shift
C	SUMWT	REAL		output	Sum of weights
C Audit trail:
C	Added sum of weights
C				T.J.Cornwell	Jan 26 1989
C	Changed to use symmetric gridding function
C				T.J.Cornwell	Nov 19 1989
C      Changed to Double for SHIFT
C				T.J.Cornwell	Jan 3 1990
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, SUPPU, OSAMPU, NV, SUPPV, OSAMPV, NW, SUPPW,
     1		OSAMPW
      INTEGER	UORIGIN, VORIGIN, WORIGIN
      COMPLEX 	VIS(*), GVIS(NU, NV, NW)
      LOGICAL	PSF
      REAL	WT(*), SUMWT
      REAL	U(*), USCALE, UOFFSET, GRIDFNU(*)
      REAL	V(*), VSCALE, VOFFSET, GRIDFNV(*)
      REAL	W(*), WSCALE, WOFFSET, GRIDFNW(*)
      DOUBLE PRECISION	SHIFT(*)
      COMPLEX	ROTVIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDH3D')
C
      INTEGER 	IVIS, NGRID, USIGN, CFOFFSET
      INTEGER	OFFU, UGRID, UCEN, DELU, DELUI
      INTEGER	OFFV, VGRID, VCEN, DELV, DELVI
      INTEGER	OFFW, WGRID, WCEN, DELW, DELWI
      REAL	UCELL, VCELL, WCELL, UVWWT, REVIS, IMVIS, LSUMWT
      COMPLEX	FVIS
      REAL	TWOPI, STOR, PHASE
C==========================================================================
      IF (ERROR) GO TO 999
C
      TWOPI = 8 * ATAN(1.0)
      STOR = TWOPI / (360.0 * 3600.0)
      CFOFFSET = (SUPPU+1)*OSAMPU + 1
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
C
C Start of loop elements to be gridded
C
      LSUMWT = 0.0
      SUMWT = 0.0
      DO 10 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 10
         NGRID = NGRID + 1
         SUMWT = SUMWT + WT(IVIS)
         UCELL = USCALE * U(IVIS) + UOFFSET 
         VCELL = VSCALE * V(IVIS) + VOFFSET 
         WCELL = WSCALE * W(IVIS) + WOFFSET 
         PHASE = TWOPI * STOR * (U(IVIS) * SHIFT(1) + V(IVIS) * SHIFT(2)
     1      + W(IVIS) * SHIFT(3))
         ROTVIS = VIS(IVIS) * CMPLX(COS(PHASE), -SIN(PHASE))
         IF (UCELL.GT.0.0) THEN
            REVIS = REAL(ROTVIS)
            IMVIS = - AIMAG(ROTVIS)
            UCELL = UCELL + UORIGIN
            VCELL = VCELL + VORIGIN
            WCELL = WCELL + WORIGIN
         ELSE
            REVIS = REAL(ROTVIS)
            IMVIS = AIMAG(ROTVIS)
            UCELL = - UCELL + UORIGIN
            VCELL = - VCELL + VORIGIN
            WCELL = - WCELL + WORIGIN
         END IF
         IF (PSF) THEN
            REVIS = 1.0
            IMVIS = 0.0
         END IF
         IF ((NINT(UCELL)+SUPPU).GT.NU) GO TO 10
         IF ((NINT(VCELL)+SUPPV).GT.NV) GO TO 10
         IF ((NINT(VCELL)-SUPPV).LT.1) GO TO 10
         IF ((NINT(WCELL)+SUPPW).GT.NW) GO TO 10
         IF ((NINT(WCELL)-SUPPW).LT.1) GO TO 10
         DELUI = NINT(OSAMPU*(FLOAT(NINT(UCELL))-UCELL))
         DELVI = NINT(OSAMPV*(FLOAT(NINT(VCELL))-VCELL))
         DELWI = NINT(OSAMPW*(FLOAT(NINT(WCELL))-WCELL))
         UCEN = NINT(UCELL) - UORIGIN
         VCEN = NINT(VCELL) - VORIGIN
         WCEN = NINT(WCELL) - WORIGIN
C
C Now do inner loop over convolution functions
C
         DO 100 OFFU = - SUPPU, SUPPU
            USIGN = SIGN(1, (UCEN + OFFU))
            FVIS = CMPLX(REVIS, USIGN * IMVIS)
            DELU  = DELUI + OSAMPU*OFFU + CFOFFSET
            UGRID = UORIGIN + USIGN * (UCEN + OFFU)
            DO 110 OFFW = -SUPPW, SUPPW
               DELW = DELWI + OSAMPW*OFFW + CFOFFSET
               WGRID = WORIGIN + USIGN * (WCEN + OFFW)
               DO 120 OFFV = - SUPPV, SUPPV
                  DELV  = DELVI + OSAMPV*OFFV + CFOFFSET
                  VGRID = VORIGIN + USIGN * (VCEN + OFFV)
                  UVWWT = WT(IVIS) * GRIDFNU(DELU) * GRIDFNV(DELV) *
     1               GRIDFNW(DELW)
                  GVIS(UGRID,VGRID,WGRID) = GVIS(UGRID,VGRID,WGRID) + 
     1                  UVWWT * FVIS
                  LSUMWT = LSUMWT + UVWWT
 120            CONTINUE
 110         CONTINUE
C
C Take care of points on the U=0 plane: we need to reflect these through
C the origin.
C
            IF ((UCEN+OFFU).EQ.0) THEN
               USIGN = - USIGN
               FVIS = CMPLX(REVIS, USIGN * IMVIS)
               DO 130 OFFW = -SUPPW, SUPPW
                  DELW = DELWI + OSAMPW*OFFW + CFOFFSET
                  WGRID = WORIGIN + USIGN * (WCEN + OFFW)
                  DO 140 OFFV = - SUPPV, SUPPV
                     DELV  = DELVI + OSAMPV*OFFV + CFOFFSET
                     VGRID = VORIGIN + USIGN * (VCEN + OFFV)
                     UVWWT = WT(IVIS) * GRIDFNU(DELU) * GRIDFNV(DELV) *
     1                  GRIDFNW(DELW)
                     GVIS(UGRID,VGRID,WGRID) = GVIS(UGRID,VGRID,WGRID) + 
     1                  UVWWT * FVIS
 140               CONTINUE
 130            CONTINUE
             END IF
 100      CONTINUE
C
  10  CONTINUE
C
C
      IF (LSUMWT.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No weight!')
         GO TO 999
      ELSE
         LSUMWT = FLOAT(NU-1)*FLOAT(NV)*FLOAT(NW)/LSUMWT
         DO 70 WGRID = 1, NW
            DO 60 VGRID = 1, NV
               DO 50 UGRID = 1, NU
                  GVIS(UGRID, VGRID, WGRID) = LSUMWT * 
     1               GVIS(UGRID, VGRID, WGRID)
  50           CONTINUE
  60        CONTINUE
  70     CONTINUE
      END IF
C
 999  CONTINUE
      END
