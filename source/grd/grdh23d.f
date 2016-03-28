C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdh23d.f	1.3    11/7/90
C
      SUBROUTINE GRDH23D (VIS, WT, U, V, W, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, UORIGIN, VORIGIN, GVIS, PSF, 
     2   NU, NV, GRIDFNU, SUPPU, OSAMPU, GRIDFNV, SUPPV, OSAMPV, NZ, 
     3   CELLZ, REFZ, SHIFT, SUMWT)
C
CD Grid two dimensional complex data. There must be no points within the 
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
C	PSF	LOG		input	TRUE for PSF
C	NU	INT		input	Size of gridded plane
C	GRIDFNU	REAL		input	Gridding function
C	SUPPU	INT		input	Support of gridding function
C	OSAMPU	INT		input	Over-sampling factor
C	NZ	INT		input	Number of planes in Z
C	CELLZ	REAL		input	Cellsize in Z
C	REFZ	REAL		input	Reference pixel in Z
C	SHIFT	REAL		input	Shift matrix
C	SUMWT	REAL		output	Sum of weights
C Audit trail:
C	Added sum of weights
C				T.J.Cornwell	Jan 26 1989
C	Changed to shift matrix
C				T.J.Cornwell	Feb 13 1989
C	Changed to use symmetric gridding function
C				T.J.Cornwell	Nov 19 1989
C      Changed to Double for SHIFT
C				T.J.Cornwell	Jan 3 1990
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, SUPPU, OSAMPU, NV, SUPPV, OSAMPV, NZ
      INTEGER	UORIGIN, VORIGIN
      COMPLEX 	VIS(*), GVIS(NU, NV, NZ)
      LOGICAL	PSF
      REAL	WT(*), SUMWT
      REAL	U(*), USCALE, UOFFSET, GRIDFNU(*)
      REAL	V(*), VSCALE, VOFFSET, GRIDFNV(*)
      REAL	W(*), REFZ, CELLZ
      DOUBLE PRECISION	SHIFT(3,*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDH23D')
C
      INTEGER 	IVIS, NGRID, USIGN, IZ, CFOFFSET
      INTEGER	OFFU, UGRID, UZERO, DELU, DELUI
      INTEGER	OFFV, VGRID, VZERO, DELV, DELVI
      REAL	UCELL, VCELL, UVWT, REVIS, IMVIS, PHASE, TWOPI, DTOR,
     1		LSUMWT
      REAL	STOR, ULOCAL, VLOCAL, WLOCAL
      COMPLEX	FVIS, ROTFVIS
C==========================================================================
      IF (ERROR) GO TO 999
C
      TWOPI = 8 * ATAN(1.0)
      STOR = TWOPI / (360.0 * 3600.0)
      CFOFFSET = (SUPPU+1)*OSAMPU + 1
C
      DO 7 IZ = 1, NZ
         DO 6 VGRID = 1, NV
            DO 5 UGRID = 1, NU
               GVIS(UGRID,VGRID,IZ) = 0.0
  5         CONTINUE
  6      CONTINUE
  7   CONTINUE
C
      DTOR = TWOPI / (360.0)
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
         ULOCAL = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     1      SHIFT(3,1) * W(IVIS)
         VLOCAL = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     1      SHIFT(3,2) * W(IVIS)
         WLOCAL = SHIFT(1,3) * U(IVIS) + SHIFT(2,3) * V(IVIS) +
     1      SHIFT(3,3) * W(IVIS)
         PHASE = TWOPI * (WLOCAL-W(IVIS))
         FVIS = VIS(IVIS) * CMPLX(COS(PHASE), -SIN(PHASE))
         UCELL = USCALE * ULOCAL + UOFFSET 
         VCELL = VSCALE * VLOCAL + VOFFSET 
         IF (UCELL.GT.0.0) THEN
            FVIS = CONJG(FVIS)
            UCELL = UCELL + FLOAT(UORIGIN)
            VCELL = VCELL + FLOAT(VORIGIN)
         ELSE
            UCELL = - UCELL + FLOAT(UORIGIN)
            VCELL = - VCELL + FLOAT(VORIGIN)
            WLOCAL = - WLOCAL
         END IF
         IF (PSF) THEN
            FVIS = 1.0
         END IF
         IF ((NINT(UCELL)+2*SUPPU).GT.NU) GO TO 10
         IF ((NINT(VCELL)+SUPPV).GT.NV) GO TO 10
         IF ((NINT(VCELL)-SUPPV).LT.1) GO TO 10
         DELUI = NINT(OSAMPU*(FLOAT(NINT(UCELL))-UCELL))
         DELVI = NINT(OSAMPV*(FLOAT(NINT(VCELL))-VCELL))
C
C Find the phase-rotated visibility for each plane
C
C There is no problem with running into an axis so just plunge right
C in. Remember that at this stage we shift everything over by SUPPU
C cells so that we can ignore the edge effects on the v-axis
C
         UZERO = NINT(UCELL) + SUPPU
         VZERO = NINT(VCELL)
C
C Do one plane at a time
C
         DO 155 IZ = 1, NZ
            PHASE = - TWOPI * WLOCAL * DTOR * CELLZ * 
     1         (FLOAT(IZ)-REFZ)
            ROTFVIS = FVIS * CMPLX(COS(PHASE), -SIN(PHASE))
            DO 140 OFFV = - SUPPV, SUPPV
               DELV  = DELVI + OSAMPV*OFFV + CFOFFSET
               VGRID = VZERO + OFFV
               DO 150 OFFU = - SUPPU, SUPPU
                  DELU  = DELUI + OSAMPU*OFFU + CFOFFSET
                  UGRID = UZERO + OFFU
                  UVWT = WT(IVIS) * GRIDFNU(DELU) * GRIDFNV(DELV) 
                  GVIS(UGRID,VGRID,IZ) = GVIS(UGRID,VGRID,IZ) + 
     1               UVWT * ROTFVIS
                  LSUMWT = LSUMWT + UVWT
 150           CONTINUE
 140        CONTINUE
 155     CONTINUE
C
  10  CONTINUE
C
      IF (LSUMWT.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No weight!')
         GO TO 999
      ELSE
C
C Symmetrize
C
         UZERO = UORIGIN + SUPPU
         VZERO = VORIGIN
         DO 75 IZ = 1, NZ
            DO 70 OFFV = -NV/2+1, NV/2-1
               DO 60 OFFU = 1, SUPPU
                  GVIS(UZERO+OFFU,VZERO+OFFV,IZ) =
     1               GVIS(UZERO+OFFU,VZERO+OFFV,IZ) +
     2               CONJG(GVIS(UZERO-OFFU,VZERO-OFFV,IZ))
  60           CONTINUE
  70        CONTINUE
  75     CONTINUE
         DO 85 IZ = 1, NZ
            DO 80 OFFV = 0, NV/2-1
               GVIS(UZERO,VZERO+OFFV,IZ) = GVIS(UZERO,VZERO+OFFV,IZ) +
     1            CONJG(GVIS(UZERO,VZERO-OFFV,IZ))
               GVIS(UZERO,VZERO-OFFV,IZ) = 
     1            CONJG(GVIS(UZERO,VZERO+OFFV,IZ))
  80        CONTINUE
  85     CONTINUE
C
C Shift left and normalize
C
         LSUMWT = FLOAT(NU-1)*FLOAT(NV)*FLOAT(NZ)/LSUMWT
         DO 55 IZ = 1, NZ
            DO 50 VGRID = 1, NV
               DO 40 UGRID = 1, NU-SUPPU
                  GVIS(UGRID,VGRID,IZ) =
     1               LSUMWT * GVIS(UGRID+SUPPU,VGRID,IZ)
  40           CONTINUE
               DO 46 UGRID = NU-SUPPU+1, NU
                  GVIS(UGRID,VGRID,IZ) = 0.0
  46           CONTINUE
  50        CONTINUE
  55     CONTINUE
      END IF
C
 999  CONTINUE
      END
