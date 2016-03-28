C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrsl2d.f	1.1    5/7/94
C
      SUBROUTINE PIXRSL2D (IMG, NX, NY, X1, X2, Y1, Y2, NP,
     $   ORIGIN, SX, SY, MORDIN, HANDLE)
C
CD Slice through a 2D real image
C
C	IMG	REAL(N1,N2)	input	Image
C	N1,N2	INT		input	Size of Image
C	X1,X2	REAL		input	Limits in first axis (pixels)
C	Y1,Y2	REAL		input	Limits in second axis (pixels)
C	NP	INT		input	Number of points in slice
C	ORIGIN	REAL		input	Slice abscissa at X1,Y1
C	SX, SY	REAL		input	Scale -- arcsec/pixel in X & Y
C	MORDIN	INT		input	Interpolator order
C	HANDLE	CH*(*)		input	Handle for output stream
C
C
C The interpolator is fairly casual, and this shouldn't be used for the
C most demanding of applications.  It merely uses a (2*MORD-1)th order
C polynomial over the 2*MORDx2*MORD grid of cells containing each target
C point, interpolating the y scan lines to the desired x, and then
C interpolating that in y.  It works well enough for most things.
C If MORD<0, then a bicubic spline is used instead, which might be more
C stable over large support boxes if the outlying points chance rapidly.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Feb 26 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	HANDLE
      INTEGER		NX, NY, MORDIN
      REAL		IMG(NX,NY), X1, X2, Y1, Y2, ORIGIN, SX, SY
      INTEGER		NP
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRSL2D')
C
      INTEGER		MORDMAX, M2MAX
      PARAMETER		(MORDMAX=6)
      PARAMETER		(M2MAX=2*MORDMAX)
C
      INTEGER		I, J, K, IOFF, JOFF, MORD, M2
      REAL		X, Y, DX, DY, BUF(M2MAX*M2MAX), XBUF(M2MAX),
     $   		YBUF(M2MAX), F, DF, F2(M2MAX*M2MAX), Z, DZ
      LOGICAL		DOPOLY
C=====================================================================
      IF (ERROR) GO TO 999
C
      MORD = ABS(MORDIN)
      IF (MORD.GT.MORDMAX) THEN
         WRITE (MESSAGE, 1000) MORDMAX
 1000    FORMAT ('Order reduced to ',I1)
         CALL MSGPUT (MESSAGE, 'W')
         MORD = MORDMAX
      END IF
C
      DOPOLY = (MORDIN .GT. 0)
      M2 = 2 * MORD
      X = X1
      Y = Y1
      DX = (X2 - X1) / (NP - 1)
      DY = (Y2 - Y1) / (NP - 1)
      DZ = SQRT((DX*SX)**2 + (DY*SY)**2)
      Z = ORIGIN
C
      DO 300 K = 1, NP
C
         IOFF = NINT(X)-MORD-1
         IOFF = MAX(0, MIN(NX-MORD, IOFF))
C
         JOFF = NINT(Y)-MORD-1
         JOFF = MAX(0, MIN(NY-MORD, JOFF))
C
         DO 110 J = 1, M2
            DO 100 I = 1, M2
               BUF((J-1)*M2+I) = IMG(I+IOFF,J+JOFF)
 100        CONTINUE
            YBUF(J) = J+JOFF
 110     CONTINUE
         DO 120 I = 1, M2
            XBUF(I) = I+IOFF
 120     CONTINUE
C
C Call the NR 2D interpolator, polynomial or bicubic spline
C
         IF (DOPOLY) THEN
            CALL POLIN2 (XBUF, YBUF, BUF, M2, M2, X, Y, F, DF)
         ELSE
            CALL SPLIE2 (XBUF, YBUF, BUF, M2, M2, F2)
            CALL SPLIN2 (XBUF, YBUF, BUF, F2, M2, M2, X, Y, F)
         END IF
C
         WRITE (MESSAGE, 1120) Z, F, K
 1120    FORMAT (2X,2E14.5,I5)
         CALL TXTWRITE (HANDLE, MESSAGE)
C
         X = X + DX
         Y = Y + DY
         Z = Z + DZ
 300  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
