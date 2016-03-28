C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdrfel.f	1.2    11/16/92
C
      SUBROUTINE PIXDRFEL (F, NX, NY, XC, YC, RX, RY, THETA, VAL)
C
CD Draw a filled ellipse into an array
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
C This is not a particularly efficient implementation, as it uses real
C math and solves the quadratic equation for every raster scan line.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Nov 23 1991
C	Ooops!  Didn't include edge clipping on vertical scan lines.
C				D.S.Briggs	Nov 14 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY
      REAL		F(NX,NY)
      REAL		XC, YC, RX, RY, THETA, VAL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXDRFEL')
C
      INTEGER		I, J, IXLO, IXHI, IY
      REAL		XLO, XHI, YP, X1, Y1, QA, QB, QC
      REAL		A2, D, A, B, ROT
C
      REAL		D2R
      PARAMETER		(D2R = 3.1415926/180.0)
C=====================================================================
      IF (ERROR) GO TO 999
C
C Ensure that major axis .ge. minor axis
C
      IF (RX.GE.RY) THEN
         A = RX
         B = RY
         ROT = THETA
      ELSE
         A = RY
         B = RX
         ROT = THETA + 90.0
      END IF
C
      A2 = A**2
      X1 = SQRT(A2 - B**2) * COS(ROT * D2R)
      Y1 = SQRT(A2 - B**2) * SIN(ROT * D2R)
C
      DO 200 I = -INT(A)-1, INT(A)+1
         IY = NINT(YC) + I
         IF ((IY.GE.1).AND.(IY.LE.NY)) THEN
            YP = IY - YC
            QA = X1**2 - A2
            QB = 2*X1*Y1*YP
            QC = YP**2*(Y1**2-A2) + A2*(A2-X1**2-Y1**2)
            D = QB**2 - 4*QA*QC
            IF (D.GT.0.0) THEN
               XLO = XC + (-QB + SQRT(D)) / (2*QA)
               XHI = XC + (-QB - SQRT(D)) / (2*QA)
               IXLO = MAX(1, MIN(NX, INT(XLO)+1))
               IXHI = MAX(1, MIN(NX, INT(XHI)))
               DO 100 J = IXLO, IXHI
                  F(J, IY) = VAL
 100           CONTINUE
            END IF
         END IF
 200  CONTINUE
C
 999  CONTINUE
      END

