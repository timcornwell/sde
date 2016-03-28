C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grddeh23.f	1.3    11/7/90
C
      SUBROUTINE GRDDEH23 (VIS, WT, U, V, W, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, UORIGIN, VORIGIN, GVIS, 
     2   NU, NV, INTFNU, SUPPU, OSAMPU, INTFNV, SUPPV, OSAMPV, NZ, 
     3   CELLZ, REFZ, SHIFT)
C
CD DeGrid two dimensional complex data. There must be no points within the 
C support size of the edge of the grid. This version will properly grid only
C Hermitean data. The current timing is about 400 microsec per point to
C be gridded onto a 1024**2 grid on the CONVEX C-1.
C The scaling factors should be set so that USCALE*U + UOFFSET converts
C to grid cells centered at 0. This is then shifted to UORIGIN. Thus,
C for example, UOFFSET, VOFFSET should nearly always be zero, while
C UORIGIN = 1, VORIGIN = NV/2.
C
C	VIS	CMPLX(*)	input	Non-gridded data
C	WT	REAL(*)		input	Weights
C	U	REAL(*)		input	Coordinates of data
C	V	REAL(*)		input	Coordinates of data
C	NVIS	INT		input	Number to be gridded
C	USCALE	REAL		input	Scaling factor to get to pixels
C	UOFFSET	REAL		input	Offset to get to pixels
C	UORIGIN	INT		input	Origin of u axis
C	GVIS	CMPLX(*)	output	Gridded data
C	NU	INT		input	Size of gridded plane
C	INTFNU	REAL		input	Gridding function
C	SUPPU	INT		input	Support of gridding function
C	OSAMPU	INT		input	Over-sampling factor
C	NZ	INT		input	Number of planes in Z
C	CELLZ	REAL		input	Cellsize in Z
C	REFZ	REAL		input	Reference pixel in Z
C	SHIFT	REAL		input	Shift matrix
C Audit trail:
C	Changed to rotation matrix
C				T.J.Cornwell	Feb 13 1989
C	Fixed normalization by NORM. Also do not need to sum SUMWT
C	inside loops over Z
C				T.J. Cornwel	Feb 18 1989
C	Changed to use symmetric gridding function
C				T.J.Cornwell	Nov 19 1989
C      Changed to Double for SHIFT
C				T.J.Cornwell	Jan 3 1990
C	Fixed bug  which set all points too near the axis to 0.0
C				T.J.Cornwell	April 26 1990
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, SUPPU, OSAMPU, NV, SUPPV, OSAMPV, NZ
      INTEGER	UORIGIN, VORIGIN
      COMPLEX 	VIS(*), GVIS(NU, NV, NZ)
      LOGICAL	PSF
      REAL	WT(*)
      REAL	U(*), USCALE, UOFFSET, INTFNU(*)
      REAL	V(*), VSCALE, VOFFSET, INTFNV(*)
      REAL	W(*), REFZ, CELLZ
      DOUBLE PRECISION	SHIFT(3,*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDDEH23')
C
      INTEGER 	IVIS, NGRID, USIGN, IZ, NBAD, CFOFFSET
      INTEGER	OFFU, UINT, UCEN, DELU, DELUI
      INTEGER	OFFV, VINT, VCEN, DELV, DELVI
      REAL	UCELL, VCELL, UVWT, REVIS, IMVIS, PHASE, TWOPI, DTOR,
     1		ULOCAL, VLOCAL, WLOCAL, SUMWT
      REAL	STOR
      COMPLEX	FVIS, CROTZ(128), ROT
C==========================================================================
      IF (ERROR) GO TO 999
C
      TWOPI = 8 * ATAN(1.0)
      STOR = TWOPI / (360.0 * 3600.0)
      CFOFFSET = (SUPPU+1)*OSAMPU + 1
C
      DTOR = TWOPI / (360.0)
      NBAD = 0
C
C Start of loop elements to be de-gridded
C
      DO 10 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 10
         VIS(IVIS) = 0.0
         SUMWT = 0.0
         WT(IVIS) = - WT(IVIS)
         ULOCAL = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     1      SHIFT(3,1) * W(IVIS)
         VLOCAL = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     1      SHIFT(3,2) * W(IVIS)
         WLOCAL = SHIFT(1,3) * U(IVIS) + SHIFT(2,3) * V(IVIS) +
     1      SHIFT(3,3) * W(IVIS)
         PHASE = TWOPI * (WLOCAL - W(IVIS))
         ROT = CMPLX(COS(PHASE), SIN(PHASE))
         UCELL = USCALE * ULOCAL + UOFFSET 
         VCELL = VSCALE * VLOCAL + VOFFSET 
         IF (UCELL.GT.0.0) THEN
            UCELL = UCELL + FLOAT(UORIGIN)
            VCELL = VCELL + FLOAT(VORIGIN)
         ELSE
            UCELL = - UCELL + FLOAT(UORIGIN)
            VCELL = - VCELL + FLOAT(VORIGIN)
            WLOCAL = - WLOCAL
         END IF
         IF ((NINT(UCELL)+SUPPU).GT.NU) GO TO 10
         IF ((NINT(VCELL)+SUPPV).GT.NV) GO TO 10
         IF ((NINT(VCELL)-SUPPV).LT.1) GO TO 10
         DELUI = NINT(OSAMPU*(FLOAT(NINT(UCELL))-UCELL))
         DELVI = NINT(OSAMPV*(FLOAT(NINT(VCELL))-VCELL))
         UCEN = NINT(UCELL) - UORIGIN
         VCEN = NINT(VCELL) - VORIGIN
C
C Make phase rotation vectors
C
         DO 45 IZ = 1, NZ
            PHASE = - TWOPI * WLOCAL * DTOR * CELLZ * 
     1         (FLOAT(IZ)-REFZ)
            CROTZ(IZ) = CMPLX(COS(PHASE), SIN(PHASE))
  45     CONTINUE
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
                     UVWT =  INTFNU(DELU) * INTFNV(DELV)
                     SUMWT = SUMWT + UVWT
                     DO 115 IZ = 1, NZ
                        IF (USIGN.GT.0) THEN
                           FVIS = GVIS(UINT,VINT,IZ)
                        ELSE
                           FVIS = CONJG(GVIS(UINT,VINT,IZ))
                        END IF
                        VIS(IVIS) = VIS(IVIS) + FVIS * CROTZ(IZ) * UVWT
 115                 CONTINUE
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
                     UVWT =  INTFNU(DELU) * INTFNV(DELV)
                     SUMWT = SUMWT + UVWT
                     DO 125 IZ = 1, NZ
                        VIS(IVIS) = VIS(IVIS) + 
     1                     GVIS(UINT, VINT, IZ) * UVWT * CROTZ(IZ)
 125                 CONTINUE
 120              CONTINUE
               END IF
 100        CONTINUE
         ELSE
C
C No danger of axis problems: simple loop. Most points will go through
C this loop
C
            DO 140 OFFV = - SUPPV, SUPPV
               DELV  = DELVI + OSAMPV*OFFV + CFOFFSET
               VINT = VORIGIN + VCEN + OFFV
               DO 150 OFFU = - SUPPU, SUPPU
                  DELU  = DELUI + OSAMPU*OFFU + CFOFFSET
                  UINT = UORIGIN + UCEN + OFFU
                  UVWT = INTFNU(DELU) * INTFNV(DELV) 
                  SUMWT = SUMWT + UVWT
                  DO 160 IZ = 1, NZ
                     VIS(IVIS) = VIS(IVIS) + GVIS(UINT, VINT, IZ) * 
     1                  UVWT * CROTZ (IZ)
 160              CONTINUE
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
C Now phase rotate
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
