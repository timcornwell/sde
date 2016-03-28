C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdh1d.f	1.4    5/20/93
C
      SUBROUTINE GRDH1D (VIS, WT, U, NVIS, USCALE, UOFFSET, UORIGIN, 
     1   GVIS, PSF, NU, GRIDFNU, SUPPU, OSAMPU, SUMWT)
C
CD Grid one dimensional complex data. Points within the support size of the
C edge of the grid are ignored. This version will properly grid 
C Hermitean data.
C
C   1   GVIS, PSF, NU, GRIDFNU, SUPPU, OSAMPU, SUMWT)
C
C	VIS	CMPLX(*)	input	Non-gridded data
C	WT	REAL(*)		input	Weights
C	U	REAL(*)		input	Coordinates of data
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
C	SUMWT	REAL		output	Sum of gridding weights
C Audit trail:
C	Added sum of gridding weights
C				T.J.Cornwell	Jan 26 1989
C	Changed to use symmetric gridding function
C				T.J.Cornwell	Nov 19 1989
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      COMPLEX 	VIS(*), GVIS(*)
      LOGICAL	PSF
      REAL	U(*), USCALE, UOFFSET, GRIDFNU(*), WT(*), SUMWT
      INTEGER	UORIGIN
      INTEGER	NVIS, NU, SUPPU, OSAMPU
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDH1D')
C
      INTEGER 	IVIS, OFFU, UGRID, UCEN, DELU, DELUI, USIGN, CFOFFSET
      REAL	UCELL
      COMPLEX	AVIS, FVIS
      REAL	LSUMWT
C
C==========================================================================
      IF (ERROR) GO TO 999
C
      CFOFFSET = (SUPPU+1)*OSAMPU + 1
C
      DO 5 UGRID = 1, NU
         GVIS(UGRID) = 0.0
  5   CONTINUE
C
      LSUMWT = 0.0
      DO 10 IVIS = 1, NVIS
         IF(WT(IVIS).GT.0.0)  THEN
            UCELL = USCALE * U(IVIS) + UOFFSET
            IF (UCELL.GT.0.0) THEN
               UCELL = UCELL + FLOAT(UORIGIN)
               AVIS = CONJG(VIS(IVIS))
            ELSE
               UCELL = - UCELL + FLOAT(UORIGIN)
               AVIS = VIS(IVIS)
            END IF
            IF (PSF) AVIS = 1.0
            UCEN = NINT(UCELL)
            IF (ABS(UCEN+SUPPU).GT.NU) GO TO 10
            DELUI = NINT(OSAMPU*(FLOAT(UCEN)-UCELL))
            DO 20 OFFU = - SUPPU, -1
               UGRID = UCEN + OFFU - UORIGIN
               USIGN = SIGN(1, UGRID)
               UGRID = UORIGIN + USIGN * UGRID
               FVIS = CMPLX(REAL(AVIS), USIGN*AIMAG(AVIS))
               DELU  = DELUI + OFFU*OSAMPU + CFOFFSET
               GVIS(UGRID) = GVIS(UGRID) + WT(IVIS) * FVIS *
     1            GRIDFNU(DELU)
               LSUMWT = LSUMWT + WT(IVIS) * GRIDFNU(DELU)
 20         CONTINUE
            DO 21 OFFU = 0, SUPPU
               UGRID = UCEN + OFFU
               DELU  = DELUI + OFFU*OSAMPU + CFOFFSET
               GVIS(UGRID) = GVIS(UGRID) + WT(IVIS) * AVIS *
     1            GRIDFNU(DELU)
               LSUMWT = LSUMWT + WT(IVIS) * GRIDFNU(DELU)
 21         CONTINUE
         END IF
 10   CONTINUE
C
      SUMWT = LSUMWT
      IF (LSUMWT.EQ.0.0) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'No weight!')
         GO TO 999
      ELSE
         LSUMWT = FLOAT(NU-1)/LSUMWT
         DO 30 UGRID = 1, NU
            GVIS(UGRID) = LSUMWT*GVIS(UGRID)
  30     CONTINUE
      END IF
C
 999  CONTINUE
      END
