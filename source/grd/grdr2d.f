C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdr2d.f	1.3    11/7/90
C
      SUBROUTINE GRDR2D (VIS, WT, U, V, NVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, UORIGIN, VORIGIN, GVIS, NU, NV,
     2   GRIDFNU, SUPPU, OSAMPU, GRIDFNV, SUPPV, OSAMPV, SUMWT)
C
CD Grid two dimensional real data. 
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
C	GRIDFNU	REAL		input	Gridding function
C	SUPPU	INT		input	Support of gridding function
C	OSAMPU	INT		input	Over-sampling factor
C	GRIDFNV	REAL		input	Gridding function
C	SUPPV	INT		input	Support of gridding function
C	OSAMPV	INT		input	Over-sampling factor
C	SUMWT	REAL		output	Sum of weights
C Audit trail:
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, SUPPU, OSAMPU, NV, SUPPV, OSAMPV
      INTEGER	UORIGIN, VORIGIN
      REAL	VIS(*), GVIS(NU, NV)
      REAL	WT(*)
      REAL	U(*), USCALE, UOFFSET, GRIDFNU(*)
      REAL	V(*), VSCALE, VOFFSET, GRIDFNV(*)
      REAL	SUMWT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDR2D')
C
      INTEGER 	IVIS, NGRID, CFOFFSET
      INTEGER	OFFU, UGRID, DELU, DELUI, UZERO
      INTEGER	OFFV, VGRID, DELV, DELVI, VZERO
      REAL	UCELL, VCELL, UVWT, LSUMWT
      REAL	FVIS
C==========================================================================
      IF (ERROR) GO TO 999
C
      CFOFFSET = (SUPPU+1)*OSAMPU + 1
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
         NGRID = NGRID + 1
         FVIS = VIS(IVIS)
         UCELL = USCALE * U(IVIS) + UOFFSET 
         VCELL = VSCALE * V(IVIS) + VOFFSET 
         SUMWT = SUMWT + WT(IVIS)
         UCELL = UCELL + FLOAT(UORIGIN)
         VCELL = VCELL + FLOAT(VORIGIN)
C
C Check for boundaries
C
         IF ((NINT(UCELL)+SUPPU).GT.NU) GO TO 10
         IF ((NINT(VCELL)+SUPPV).GT.NV) GO TO 10
         IF ((NINT(UCELL)-SUPPU).LT.1) GO TO 10
         IF ((NINT(VCELL)-SUPPV).LT.1) GO TO 10
         DELUI = NINT(OSAMPU*(FLOAT(NINT(UCELL))-UCELL))
         DELVI = NINT(OSAMPV*(FLOAT(NINT(VCELL))-VCELL))
C
C There is no problem with running into an axis so just plunge right
C in. 
C
         UZERO = NINT(UCELL)
         VZERO = NINT(VCELL)
         DO 140 OFFV = - SUPPV, SUPPV
            DELV  = DELVI + OSAMPV*OFFV + CFOFFSET
            VGRID = VZERO + OFFV
            DO 150 OFFU = - SUPPU, SUPPU
               DELU  = DELUI + OSAMPU*OFFU + CFOFFSET
               UGRID = UZERO + OFFU
               UVWT = WT(IVIS) * GRIDFNU(DELU) * GRIDFNV(DELV) 
               GVIS(UGRID,VGRID) = GVIS(UGRID,VGRID) + UVWT * FVIS
               LSUMWT = LSUMWT + UVWT
 150        CONTINUE
 140     CONTINUE
C
  10  CONTINUE
C
      IF (LSUMWT.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No weight!')
         GO TO 999
      ELSE
C
C Normalize
C
         LSUMWT = FLOAT(NU)*FLOAT(NV)/LSUMWT
         DO 50 VGRID = 1, NV
            DO 40 UGRID = 1, NU
               GVIS(UGRID,VGRID) = LSUMWT * GVIS(UGRID,VGRID)
  40        CONTINUE
  50     CONTINUE
      END IF
C
 999  CONTINUE
      END
