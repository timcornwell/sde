C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdh2d.f	1.5    21 Feb 1995
C
      SUBROUTINE GRDH2D (VIS, WT, U, V, W, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, UORIGIN, VORIGIN, GVIS, PSF, NU, NV,
     2   GRIDFNU, SUPPU, OSAMPU, GRIDFNV, SUPPV, OSAMPV, SHIFT, SUMWT)
C
CD Grid two dimensional complex data.  This version will properly grid only
C Hermitean data. The current timing is about 400 microsec per point to
C be gridded onto a 128**2 grid on the CONVEX C-1.
C The scaling factors should be set so that USCALE*U + UOFFSET converts
C to grid cells centered at 0. This is then shifted to UORIGIN. Thus,
C for example, UOFFSET, VOFFSET should nearly always be zero, while
C UORIGIN = 1, VORIGIN = NV/2.
C   The phase center of the data can be adjusted as it is gridded using
C the shift matrix.
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
C	GRIDFNU	REAL		input	Gridding function
C	SUPPU	INT		input	Support of gridding function
C	OSAMPU	INT		input	Over-sampling factor
C	GRIDFNV	REAL		input	Gridding function
C	SUPPV	INT		input	Support of gridding function
C	OSAMPV	INT		input	Over-sampling factor
C	SHIFT	REAL		input	Shift matrix
C	SUMWT	REAL		output	Sum of weights
C Audit trail:
C	Now add UVWT on-axis
C				T.J.Cornwell	Jan 10 1989
C	Return SUMWT
C				T.J.Cornwell	Jan 27 1989
C	Changed to shift matrix
C				T.J. Cornwell	Feb 13 1989
C	Shift as we grid to ignore zero. This makes the gridding faster
C	but we lose the last few columns of data
C				T.J. Cornwell	Feb 16 1989
C       Changed to Double for SHIFT
C				T.J.Cornwell	Jan 3 1990
C	Some retyping to neaten up
C				T.J.Cornwell	Mar 4 1991
C	Added Double precision for VLBI
C				T.J.Cornwell	Feb 21 1995
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, SUPPU, OSAMPU, NV, SUPPV, OSAMPV
      INTEGER	UORIGIN, VORIGIN
      COMPLEX 	VIS(*), GVIS(NU, NV)
      LOGICAL	PSF
      REAL	WT(*)
      REAL	U(*), USCALE, UOFFSET, GRIDFNU(*)
      REAL	V(*), VSCALE, VOFFSET, GRIDFNV(*)
      REAL	W(*)
      DOUBLE PRECISION	SHIFT(3,*)
      REAL	SUMWT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDH2D')
C
      LOGICAL	DOSHIFT
      INTEGER 	IVIS, CFOFFSET
      INTEGER	OFFU, UGRID, DELU, DELUI, UCEN
      INTEGER	OFFV, VGRID, DELV, DELVI, VCEN
      COMPLEX	FVIS
      DOUBLE PRECISION	UG, VG, WG
      DOUBLE PRECISION	TWOPI, PHASE, ULOCAL, VLOCAL, WLOCAL
      DOUBLE PRECISION 	UCELL, VCELL, UVWT, LSUMWT
C==========================================================================
      IF (ERROR) GO TO 999
C
      TWOPI = 8 * ATAN(1.0)
      CFOFFSET = (SUPPU+1)*OSAMPU + 1
C
C Is there a shift?
C
      DOSHIFT = (SHIFT(1,1).NE.1.0D0).OR.(SHIFT(2,2).NE.1.0D0).OR.
     1   (SHIFT(3,3).NE.1.0D0)
C
      DO 6 VGRID = 1, NV
         DO 5 UGRID = 1, NU
            GVIS(UGRID,VGRID) = 0.0
  5      CONTINUE
  6   CONTINUE
C
C Start of loop elements to be gridded
C
      LSUMWT = 0.0
      SUMWT = 0.0
      DO 10 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 10
C
C Shift to new phase center if required.
C
         UG = DBLE(U(IVIS))
         VG = DBLE(V(IVIS))
         WG = DBLE(W(IVIS))
         IF (DOSHIFT) THEN
            ULOCAL = SHIFT(1,1) * UG + SHIFT(2,1) * VG +
     1         SHIFT(3,1) * WG
            VLOCAL = SHIFT(1,2) * UG + SHIFT(2,2) * VG +
     1         SHIFT(3,2) * WG
            WLOCAL = SHIFT(1,3) * UG + SHIFT(2,3) * VG +
     1         SHIFT(3,3) * WG
            PHASE = TWOPI * (WLOCAL - WG)
            FVIS = VIS(IVIS) * CMPLX(COS(PHASE), -SIN(PHASE))
            UCELL = USCALE * ULOCAL + UOFFSET 
            VCELL = VSCALE * VLOCAL + VOFFSET 
         ELSE
            FVIS = VIS(IVIS)
            UCELL = USCALE * UG + UOFFSET 
            VCELL = VSCALE * VG + VOFFSET 
         END IF
C
C Ensure that UCELL is positive
C
         IF (UCELL.GT.0.0) THEN
            FVIS = CONJG(FVIS)
            UCELL = UCELL + FLOAT(UORIGIN)
            VCELL = VCELL + FLOAT(VORIGIN)
         ELSE
            UCELL = - UCELL + FLOAT(UORIGIN)
            VCELL = - VCELL + FLOAT(VORIGIN)
         END IF
C
C If PSF required then reset FVIS here
C
         IF (PSF) FVIS = 1.0
C
C Check for boundaries
C
         IF ((NINT(UCELL)+2*SUPPU).GT.NU) GO TO 10
         IF ((NINT(VCELL)+SUPPV).GT.NV) GO TO 10
         IF ((NINT(VCELL)-SUPPV).LT.1) GO TO 10
C
C Accumulate sum of weights
C
         SUMWT = SUMWT + WT(IVIS)
C
C Find offsets within convolution function and center point of
C gridded point. At his point we offset the grid by SUPPU so
C that we don't have to worry about edge effects on the v-axis.
C
         DELUI = NINT(OSAMPU*(FLOAT(NINT(UCELL))-UCELL)) + CFOFFSET
         DELVI = NINT(OSAMPV*(FLOAT(NINT(VCELL))-VCELL)) + CFOFFSET
         UCEN = NINT(UCELL) + SUPPU
         VCEN = NINT(VCELL)
C
C There is no problem with running into an axis so just plunge right
C in. 
C ********************************************************************
C This loop produces different answers on the sparc and on the
C IBM6000. UCEN, VCEN, DELUI, DELVI are the same as are GRIDFNU, GRIDFNV
C The difference is that one pixel (?) is misplaced by two pixels in
C v. I don't know which is correct. This same error also afflicts
C GRDH23D. TJC March 3 1991
C
         DO 140 OFFV = - SUPPV, SUPPV
            DELV  = DELVI + OSAMPV*OFFV
            VGRID = VCEN + OFFV
            DO 150 OFFU = - SUPPU, SUPPU
               DELU  = DELUI + OSAMPU*OFFU
               UGRID = UCEN + OFFU
               UVWT = WT(IVIS) * GRIDFNU(DELU) * GRIDFNV(DELV) 
               GVIS(UGRID,VGRID) = GVIS(UGRID,VGRID) + UVWT * FVIS
               LSUMWT = LSUMWT + UVWT
 150        CONTINUE
 140     CONTINUE
C
C ********************************************************************
C
  10  CONTINUE
C
C Stop if no data were gridded
C
      IF (LSUMWT.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No weight!')
         GO TO 999
      END IF
C
C Symmetrize
C
      UCEN = UORIGIN + SUPPU
      VCEN = VORIGIN
      DO 70 OFFV = -NV/2+1, NV/2-1
         DO 60 OFFU = 1, SUPPU
            GVIS(UCEN+OFFU,VCEN+OFFV) = GVIS(UCEN+OFFU,VCEN+OFFV) +
     2         CONJG(GVIS(UCEN-OFFU,VCEN-OFFV))
  60     CONTINUE
  70  CONTINUE
      DO 80 OFFV = 0, NV/2-1
         GVIS(UCEN,VCEN+OFFV) = GVIS(UCEN,VCEN+OFFV) +
     1      CONJG(GVIS(UCEN,VCEN-OFFV))
         GVIS(UCEN,VCEN-OFFV) = CONJG(GVIS(UCEN,VCEN+OFFV))
  80  CONTINUE
C
C Shift left and normalize
C
      LSUMWT = FLOAT(NU-1)*FLOAT(NV)/LSUMWT
      DO 50 VGRID = 1, NV
         DO 40 UGRID = 1, NU-SUPPU
            GVIS(UGRID,VGRID) = LSUMWT * GVIS(UGRID+SUPPU,VGRID)
  40     CONTINUE
         DO 46 UGRID = NU-SUPPU+1, NU
            GVIS(UGRID,VGRID) = 0.0
  46     CONTINUE
  50  CONTINUE
C
 999  CONTINUE
      END
