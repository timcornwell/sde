C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdqnh2d.f	1.1	 3/15/93
C
      SUBROUTINE GRDQNH2D (WT, U, V, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, UORIGIN, VORIGIN, GVIS, NU, NV,
     2   SUMWT)
C
CD Grid two dimensional real data fast, no normalizing
C
C	VIS	REAL(*)		input	Non-gridded data
C	WT	REAL(*)		input	Weights
C	U	REAL(*)		input	Coordinates of data
C	V	REAL(*)		input	Coordinates of data
C	NVIS	INT		input	Number to be gridded
C	USCALE	REAL		input	Scaling factor to get to pixels
C	UOFFSET	REAL		input	Offset to get to pixels
C	VSCALE	REAL		input	Scaling factor to get to pixels
C	VOFFSET	REAL		input	Offset to get to pixels
C	UORIGIN	INT		input	Origin of u axis
C	VORIGIN	INT		input	Origin of v axis
C	GVIS	REAL(*)		output	Gridded data
C	NU	INT		input	Size of gridded plane
C	NV	INT		input	Size of gridded plane
C	SUMWT	REAL		output	Sum of weights
C Audit trail:
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, NV
      INTEGER	UORIGIN, VORIGIN
      REAL	GVIS(NU, NV)
      REAL	WT(*)
      REAL	U(*), USCALE, UOFFSET
      REAL	V(*), VSCALE, VOFFSET
      REAL	SUMWT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDQR2D')
C
      INTEGER 	IVIS, NGRID
      INTEGER	UGRID
      INTEGER	VGRID
      REAL	UCELL, VCELL
C==========================================================================
      IF (ERROR) GO TO 999
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
      SUMWT = 0.0
      DO 10 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 10
         NGRID = NGRID + 1
         UCELL = USCALE * U(IVIS) + UOFFSET 
         VCELL = VSCALE * V(IVIS) + VOFFSET 
         SUMWT = SUMWT + WT(IVIS)
         UCELL = UCELL + FLOAT(UORIGIN)
         VCELL = VCELL + FLOAT(VORIGIN)
C
C Check for boundaries
C
         IF (NINT(UCELL).GT.NU) GO TO 10
         IF (NINT(VCELL).GT.NV) GO TO 10
         IF (NINT(UCELL).LT.1) GO TO 10
         IF (NINT(VCELL).LT.1) GO TO 10
         UGRID = NINT(UCELL)
         VGRID = NINT(VCELL)
         GVIS(UGRID,VGRID) = GVIS(UGRID,VGRID) + WT(IVIS)
C
C And now conjugate point
C
         NGRID = NGRID + 1
         UCELL = - USCALE * U(IVIS) + UOFFSET 
         VCELL = - VSCALE * V(IVIS) + VOFFSET 
         SUMWT = SUMWT + WT(IVIS)
         UCELL = UCELL + FLOAT(UORIGIN)
         VCELL = VCELL + FLOAT(VORIGIN)
C
C Check for boundaries
C
         IF (NINT(UCELL).GT.NU) GO TO 10
         IF (NINT(VCELL).GT.NV) GO TO 10
         IF (NINT(UCELL).LT.1) GO TO 10
         IF (NINT(VCELL).LT.1) GO TO 10
         UGRID = NINT(UCELL)
         VGRID = NINT(VCELL)
         GVIS(UGRID,VGRID) = GVIS(UGRID,VGRID) + WT(IVIS)
C
  10  CONTINUE
C
 999  CONTINUE
      END
