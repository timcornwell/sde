C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdeusz0.f	1.1    6/13/94
C
      SUBROUTINE GRDEUSZ0 (U, V, W, WT, NVIS, UR, VR, SHIFT, NU, NV)
C
CD Estimate optimum bin size for 2-D ungridded weighting
C
C	U	REAL		input	Coordinates of data
C	V	REAL		input	Coordinates of data
C	W	REAL		input	Coordinates of data
C	WT	REAL		input	Data weights
C	NVIS	REAL		input	Number to be weighted
C	UR	REAL		input	Weighting radius in u
C	VR	REAL		input	Weighting radius in v
C	SHIFT	DBLE(3,3)	input	Rotation matrix
C	NU,NV	INT		in/out	Bin Size
C
C The basic gist of this routine is that $u_r / \Delta u$ should be
C approximately equal to a machine dependent constant.  Fortunately that
C constant seems pretty stable across architectures, and it should work
C reasonably well with what is compiled in here.  If we ever come across
C a significant exception, say a hypercube or massive vector processor
C we'll have to make these constants machine dependent.
C
C This routine returns sizes appropriate for a routine that
C maximizes the memory usage of the array by setting a variable origin.
C The origin is constrained to be integral, and the V range is constrained
C to be symmetric.
C
C Audit trail:
C	Original version
C				D.S.Briggs	Jan 31 1994
C------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER	NVIS, NU, NV
      REAL	UR, VR, U(NVIS), V(NVIS), W(NVIS), WT(NVIS)
      DOUBLE  PRECISION	SHIFT (3,3)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDEUSZ0')
C
      INTEGER PMAXNU, PMAXNV
      REAL OPTRATIO, BINEPS
      PARAMETER (OPTRATIO=7.6)
      PARAMETER (PMAXNU=1000, PMAXNV=2*PMAXNU)
      PARAMETER (BINEPS=.1)
C
      INTEGER	IVIS, NUP, NUN, MAXNU, MAXNV
      REAL	UMAX, VMAX, UT, VT
      LOGICAL	DOSHIFT
C
      INTEGER	UTLLINT
C==========================================================================
      IF (ERROR) GO TO 999
C
      IF (NU.LT.0) THEN
         MAXNU = ABS(NU)
      ELSE
         MAXNU = PMAXNU
      END IF
C
      IF (NV.LT.0) THEN
         MAXNV = ABS(NV)
      ELSE
         IF ((NV.EQ.0).AND.(NU.LT.0)) THEN
            MAXNV = 2*MAXNU
         ELSE
            MAXNV = PMAXNV
         END IF
      END IF
C
C Is there a shift?
C
      DOSHIFT = (SHIFT(1,1).NE.1.0D0).OR.(SHIFT(2,2).NE.1.0D0).OR.
     1   (SHIFT(3,3).NE.1.0D0)
C
C Scan for mins and max's
C
      UMAX = -1.0
      VMAX = -1.0
      DO 50 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 50
         IF (DOSHIFT) THEN
            UT = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     $         SHIFT(3,1) * W(IVIS)
            VT = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     $         SHIFT(3,2) * W(IVIS)
         ELSE
            UT = U(IVIS)
            VT = V(IVIS)
         END IF
C
         IF (ABS(UT).GT.UMAX) UMAX = ABS(UT)
         IF (ABS(VT).GT.VMAX) VMAX = ABS(VT)
 50   CONTINUE
C
      NUN = UTLLINT (OPTRATIO + BINEPS)
      NUP = UTLLINT (OPTRATIO * UMAX / UR + BINEPS)
      NV = 2 * UTLLINT (OPTRATIO * VMAX / VR + BINEPS)
C
      NU = MIN(MAXNU, NUN + NUP)
      NV = MIN(MAXNV, NV)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  RETURN
      END
C
