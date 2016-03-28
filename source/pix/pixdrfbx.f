C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdrfbx.f	1.1    12/26/91
C
      SUBROUTINE PIXDRFBX (F, NX, NY, XC, YC, RX, RY, THETA, VAL)
C
CD Draw a filled box into an array
C
C	F	R(NX,NY) output Array (frame buffer)
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	XC	REAL	input	X position of center
C	YC	REAL	input	Y position of center
C	RX	REAL	input	Radius along x axis
C	RY	REAL	input	Radius along y axis
C	THETA	REAL	input	Rotation of ellipse.
C	VAL	REAL	input	Cursor value to draw into array
C
C Rotation is in degrees, in the usual mathematical sense, (counterclockwise
C from the positive x-axis).  All other positions are in pixels.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Nov 24 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY
      REAL		F(NX,NY)
      REAL		XC, YC, RX, RY, THETA, VAL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXDRFBX')
C
      REAL		DX1, DX2, DY1, DY2, R, PHI
      REAL		X(4), Y(4), WORK(16)
C
      REAL		D2R
      PARAMETER		(D2R = 3.1415926/180.0)
C=====================================================================
      IF (ERROR) GO TO 999
C
      R = SQRT(RX**2 + RY**2) / 2.0
      PHI = ATAN(RY/RX)
      DX1 = R * COS(THETA*D2R + PHI)
      DX2 = R * COS(THETA*D2R - PHI)
      DY1 = R * SIN(THETA*D2R + PHI)
      DY2 = R * SIN(THETA*D2R - PHI)
C
      X(1) = XC + DX1
      Y(1) = YC + DY1
      X(2) = XC + DX2
      Y(2) = YC + DY2
      X(3) = XC - DX1
      Y(3) = YC - DY1
      X(4) = XC - DX2
      Y(4) = YC - DY2
C
      CALL PIXDRFPL (F, NX, NY, X, Y, 4, VAL, WORK)
C
 999  CONTINUE
      END

