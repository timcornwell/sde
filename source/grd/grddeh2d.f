C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grddeh2d.f	1.4    21 Feb 1995
C
      SUBROUTINE GRDDEH2D (VIS, WT, U, V, W, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, UORIGIN, VORIGIN, GVIS, NU, NV, 
     2   INTFNU, SUPPU, OSAMPU, INTFNV, SUPPV, OSAMPV, SHIFT)
C
CD De-Grid two dimensional complex data. There must be no points within the 
C support size of the edge of the grid. This version will degrid only
C Hermitean data. The current timing is about 165 microsec per point to
C be degridded from a 1024**2 grid on the CONVEX C-1.
C The scaling factors should be set so that USCALE*U + UOFFSET converts
C to grid cells centered at 0. This is then shifted to UORIGIN. Thus,
C for example, UOFFSET, VOFFSET should nearly always be zero, while
C UORIGIN = 1, VORIGIN = NV/2.
C
C	VIS	CMPLX(*)	Output	Un-gridded data
C	WT	REAL(*)		Output	Weights
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
C	GVIS	CMPLX(*)	input	Gridded data
C	NU	INT		input	Size of gridded plane
C	NV	INT		input	Size of gridded plane
C	INTFNU	REAL		input	Interpolation function
C	SUPPU	INT		input	Support of interpolation function
C	OSAMPU	INT		input	Over-sampling factor
C	INTFNV	REAL		input	Interpolation function
C	SUPPV	INT		input	Support of interpolation function
C	OSAMPV	INT		input	Over-sampling factor
C	SHIFT	REAL		input	Shift matrix 
C Audit trail:
C	Changed to shift matrix
C				T.J.Cornwell	Feb 13 1989
C	Changed to use symmetric gridding function
C				T.J.Cornwell	Nov 19 1989
C      Changed to Double for SHIFT
C				T.J.Cornwell	Jan 3 1990
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
      REAL	WT(*)
      REAL	U(*), USCALE, UOFFSET, INTFNU(*)
      REAL	V(*), VSCALE, VOFFSET, INTFNV(*)
      REAL	W(*)
      DOUBLE PRECISION	SHIFT(3,*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDDEH2D')
C
      INTEGER 	IVIS, USIGN, NBAD, CFOFFSET
      INTEGER	OFFU, UINT, UCEN, DELU, DELUI
      INTEGER	OFFV, VINT, VCEN, DELV, DELVI
      COMPLEX	FVIS, ROT
      REAL	UCELL, VCELL, UVWT, SUMWT
      DOUBLE PRECISION	UG, VG, WG
      DOUBLE PRECISION	TWOPI, STOR, PHASE, ULOCAL, VLOCAL, WLOCAL
C
C==========================================================================
      IF (ERROR) GO TO 999
C
      TWOPI = 8 * ATAN(1.0)
      STOR = TWOPI / (360.0 * 3600.0)
      CFOFFSET = (SUPPU+1)*OSAMPU + 1
C
      NBAD = 0
C
C Start of loop elements to be de-gridded
C
      DO 10 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 10
         VIS(IVIS) = 0.0
         SUMWT = 0.0
         WT(IVIS) = - WT(IVIS)
         UG = DBLE(U(IVIS))
         VG = DBLE(V(IVIS))
         WG = DBLE(W(IVIS))
         ULOCAL = SHIFT(1,1) * UG + SHIFT(2,1) * VG +
     1      SHIFT(3,1) * WG
         VLOCAL = SHIFT(1,2) * UG + SHIFT(2,2) * VG +
     1      SHIFT(3,2) * WG
         WLOCAL = SHIFT(1,3) * UG + SHIFT(2,3) * VG +
     1      SHIFT(3,3) * WG
         PHASE = TWOPI * (WLOCAL - WG)
         ROT = CMPLX(COS(PHASE), SIN(PHASE))
         UCELL = USCALE * ULOCAL + UOFFSET 
         VCELL = VSCALE * VLOCAL + VOFFSET 
         IF (UCELL.GT.0.0) THEN
            UCELL = UCELL + FLOAT(UORIGIN)
            VCELL = VCELL + FLOAT(VORIGIN)
         ELSE
            UCELL = - UCELL + FLOAT(UORIGIN)
            VCELL = - VCELL + FLOAT(VORIGIN)
         END IF
         IF ((NINT(UCELL)+SUPPU).GT.NU) GO TO 10
         IF ((NINT(VCELL)+SUPPV).GT.NV) GO TO 10
         IF ((NINT(VCELL)-SUPPV).LT.1) GO TO 10
         DELUI = NINT(OSAMPU*(FLOAT(NINT(UCELL))-UCELL))
         DELVI = NINT(OSAMPV*(FLOAT(NINT(VCELL))-VCELL))
         UCEN = NINT(UCELL) - UORIGIN
         VCEN = NINT(VCELL) - VORIGIN
C
C Now do inner loop over convolution functions. Split according to
C whether a point lies near the v axis.
C
         IF (NINT(UCELL).LE.SUPPU) THEN
C
C Danger of running into axis: must be careful
C
            DO 100 OFFU = - SUPPU, SUPPU
               IF (UCEN.NE.-OFFU) THEN
                  USIGN = SIGN(1, (UCEN + OFFU))
                  DELU  = DELUI + OSAMPU*OFFU + CFOFFSET
                  UINT = UORIGIN + USIGN * (UCEN + OFFU)
                  DO 110 OFFV = - SUPPV, SUPPV
                     DELV  = DELVI + OSAMPV*OFFV + CFOFFSET
                     VINT = VORIGIN + USIGN * (VCEN + OFFV)
                     UVWT = INTFNU(DELU) * INTFNV(DELV) 
                     IF (USIGN.GT.0) THEN
                        FVIS = GVIS(UINT,VINT)
                     ELSE
                        FVIS = CONJG(GVIS(UINT,VINT))
                     END IF
                     VIS(IVIS) = VIS(IVIS) + FVIS * UVWT
                     SUMWT = SUMWT + UVWT
 110              CONTINUE
C
C Take care of points on the U=0 plane: no need to get conjugate points
C
               ELSE
                  DELU  = DELUI + OSAMPU*OFFU + CFOFFSET
                  UINT = UORIGIN
                  DO 120 OFFV = - SUPPV, SUPPV
                     DELV  = DELVI + OSAMPV*OFFV + CFOFFSET
                     VINT = VORIGIN + (VCEN + OFFV)
                     UVWT = INTFNU(DELU) * INTFNV(DELV)
                     VIS(IVIS) = VIS(IVIS) + GVIS(UINT, VINT) * UVWT
                     SUMWT = SUMWT + UVWT
 120              CONTINUE
               END IF
 100        CONTINUE
         ELSE
C
C No danger of axis problems: simple loop
C
            DO 140 OFFV = - SUPPV, SUPPV
               DELV  = DELVI + OSAMPV*OFFV + CFOFFSET
               VINT = VORIGIN + VCEN + OFFV
               DO 150 OFFU = - SUPPU, SUPPU
                  DELU  = DELUI + OSAMPU*OFFU + CFOFFSET
                  UINT = UORIGIN + UCEN + OFFU
                  UVWT = INTFNU(DELU) * INTFNV(DELV) 
                  VIS(IVIS) = VIS(IVIS) + GVIS(UINT, VINT) * UVWT
                  SUMWT = SUMWT + UVWT
 150           CONTINUE
 140        CONTINUE
C
         END IF
C
C Were there any good data for this point?
C
         IF (SUMWT.GT.0.0) THEN
            VIS(IVIS) = VIS(IVIS)/SUMWT
            WT(IVIS) = ABS(WT(IVIS))
         ELSE
            NBAD = NBAD + 1
            VIS(IVIS) = 0.0
         END IF
C
C Remember that some points were reflected
C
         IF (USCALE*ULOCAL.GT.0.0) VIS(IVIS) = CONJG(VIS(IVIS))
C
C Now finally phase rotate
C
         VIS(IVIS) = VIS(IVIS) * ROT
C
  10  CONTINUE
C
      IF (NBAD.GT.0) THEN
         WRITE (MESSAGE, 1000) NBAD
 1000    FORMAT (I7,' points were not successfully degridded')
         CALL MSGPUT (MESSAGE, 'W')
      END IF
C
 999  CONTINUE
      END
