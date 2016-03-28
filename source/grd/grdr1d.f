C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdr1d.f	1.3    11/7/90
C
      SUBROUTINE GRDR1D (VIS, WT, U, NVIS, USCALE, UOFFSET, 
     1   UORIGIN, GVIS, NU, GRIDFNU, SUPPU, OSAMPU, SUMWT)
C
CD Grid one dimensional real data. 
C
C	VIS	REAL(*)		input	Non-gridded data
C	WT	REAL(*)		input	Weights
C	U	REAL(*)		input	Coordinates of data
C	NVIS	INT		input	Number to be gridded
C	USCALE	REAL		input	Scaling factor to get to pixels
C	UOFFSET	REAL		input	Offset to get to pixels
C	UORIGIN	INT		input	Origin of u axis
C	GVIS	REAL(*)		output	Gridded data
C	NU	INT		input	Size of gridded plane
C	GRIDFNU	REAL		input	Gridding function
C	SUPPU	INT		input	Support of gridding function
C	OSAMPU	INT		input	Over-sampling factor
C	SUMWT	REAL		output	Sum of weights
C Audit trail:
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER	NVIS, NU, SUPPU, OSAMPU
      INTEGER	UORIGIN
      REAL	VIS(*), GVIS(NU)
      REAL	WT(*)
      REAL	U(*), USCALE, UOFFSET, GRIDFNU(*)
      REAL	SUMWT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDR1D')
C
      INTEGER 	IVIS, NGRID, CFOFFSET
      INTEGER	OFFU, UGRID, DELU, DELUI, UZERO
      REAL	UCELL, UVWT, LSUMWT
      REAL	FVIS
C==========================================================================
      IF (ERROR) GO TO 999
C
      CFOFFSET = (SUPPU+1)*OSAMPU + 1
C
      DO 5 UGRID = 1, NU
         GVIS(UGRID) = 0.0
  5   CONTINUE
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
         SUMWT = SUMWT + WT(IVIS)
         UCELL = UCELL + FLOAT(UORIGIN)
C
C Check for boundaries
C
         IF ((NINT(UCELL)+SUPPU).GT.NU) GO TO 10
         IF ((NINT(UCELL)-SUPPU).LT.1) GO TO 10
         DELUI = NINT(OSAMPU*(FLOAT(NINT(UCELL))-UCELL))
C
C There is no problem with running into an axis so just plunge right
C in. 
C
         UZERO = NINT(UCELL)
         DO 150 OFFU = - SUPPU, SUPPU
            DELU  = DELUI + OSAMPU*OFFU + CFOFFSET
            UGRID = UZERO + OFFU
            UVWT = WT(IVIS) * GRIDFNU(DELU)
            GVIS(UGRID) = GVIS(UGRID) + UVWT * FVIS
            LSUMWT = LSUMWT + UVWT
 150     CONTINUE
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
         LSUMWT = FLOAT(NU)/LSUMWT
         DO 40 UGRID = 1, NU
            GVIS(UGRID) = LSUMWT * GVIS(UGRID)
  40     CONTINUE
      END IF
C
 999  CONTINUE
      END
