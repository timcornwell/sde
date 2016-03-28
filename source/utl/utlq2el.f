C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlq2el.f	1.3    10/22/93
C
      SUBROUTINE UTLQ2EL (A, B, C, D, E, F, XC, YC, SMAJ, SMIN, ROT)
C
CD Reduces a quadratic form to interpretable ellipse parameters
C
C                          2             2
C The quadratic form is A X  + B XY + C Y  + D X + E Y + F = 0
C
C The returned rotation is in degrees in the usual mathematical form,
C counterclockwise from the x-axis.  Everything else is unitless.
C SMAJ & SMIN are semi-major and minor axes, so multiply by 2 for
C full widths.
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	July 10 1993
C	Minus sign wrong in calculation of B1.
C				D.S.Briggs	Aug 31 1993
C	Indicate hyperbolic or mixed form with negative values
C	of SMAJ & SMIN instead of an error.
C				D.S.Briggs	Oct 21 1993
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      REAL	A, B, C, D, E, F
      REAL	XC, YC, SMAJ, SMIN, ROT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLQ2EL')
C
      REAL	PI, D2R
      PARAMETER	(PI=3.14159265359)
      PARAMETER (D2R=PI/180.0)
C
      REAL	PHI, CP, SP
      REAL	A1, B1, C1, D1, E1, A2, B2, XC1, YC1
C=======================================================================
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF ((B**2 - 4.0*A*C).EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Form is a parabola!')
         GO TO 999
      END IF
C
C Find the rotation angle that zeros the cross term
C
      IF (A.NE.C) THEN
         PHI = ATAN(B/(A-C))
         IF (PHI.LT.0.0) PHI = PHI + PI
         PHI = PHI / 2.0
      ELSE
         PHI = PI / 4.0
      END IF
      SP = SIN(PHI)
      CP = COS(PHI)
C
C Transform to rotated coordinates
C
      A1 = A*CP**2 + B*CP*SP + C*SP**2
      B1 = 2.0*(C-A)*SP*CP + B*(CP**2-SP**2)
      C1 = A*SP**2 - B*CP*SP + C*CP**2
      D1 = D*CP + E*SP
      E1 = -D*SP + E*CP
C
C Center found by completing the square in each coordinate, then rotating back
C
      XC1 = -D1/(2.0*A1)
      YC1 = -E1/(2.0*C1)
      XC = XC1*CP - YC1*SP
      YC = XC1*SP + YC1*CP
C
C Convert to form x^2/A2 + y^2/B2 = 1, in transformed coords
C
      IF ((A1.EQ.0.0).OR.(C1.EQ.0.0)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Ellipse is degenerate')
         GO TO 999
      END IF
C
      A2 = (-F + D1**2/(4.0*A1) + E1**2/(4.0*C1)) / A1
      B2 = (-F + D1**2/(4.0*A1) + E1**2/(4.0*C1)) / C1
C
      IF (ABS(A2).GE.ABS(B2)) THEN
         SMAJ = SIGN(SQRT(ABS(A2)),A2)
         SMIN = SIGN(SQRT(ABS(B2)),B2)
         ROT = PHI/D2R
      ELSE
         SMAJ = SIGN(SQRT(ABS(B2)),B2)
         SMIN = SIGN(SQRT(ABS(A2)),A2)
         ROT = PHI/D2R + 90.0
      END IF
      IF (ROT.GT.90.0) ROT = ROT - 180.0
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
