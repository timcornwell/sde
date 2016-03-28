C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrgv2d.f	1.1    11/7/94
C
      SUBROUTINE PIXRGV2D (IMG, NX, NY, PX, PY, V, MORDIN)
C
CD Get Pixel Value, Real, 2D
C
C	IMG	REAL(NY,NY)	input	Image
C	NX,NY	INT		input	Size of Image
C	PX, PY	REAL		input	Pixels, need not be integral
C	V	REAL		input	Interpolated value
C	MORDIN	INT		input	Interpolator order
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
C				D.S.Briggs	July 30 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, MORDIN
      REAL		IMG(NX,NY), PX, PY, V
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRGV2D')
C
      INTEGER		MORDMAX, M2MAX
      PARAMETER		(MORDMAX=6)
      PARAMETER		(M2MAX=2*MORDMAX)
C
      INTEGER		I, J, IOFF, JOFF, MORD, M2
      REAL		BUF(M2MAX*M2MAX), XBUF(M2MAX),
     $   		YBUF(M2MAX), DV, F2(M2MAX*M2MAX)
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
C
      IOFF = NINT(PX)-MORD-1
      IOFF = MAX(0, MIN(NX-MORD, IOFF))
C
      JOFF = NINT(PY)-MORD-1
      JOFF = MAX(0, MIN(NY-MORD, JOFF))
C
      DO 110 J = 1, M2
         DO 100 I = 1, M2
            BUF((J-1)*M2+I) = IMG(I+IOFF,J+JOFF)
 100     CONTINUE
         YBUF(J) = J+JOFF
 110  CONTINUE
      DO 120 I = 1, M2
         XBUF(I) = I+IOFF
 120  CONTINUE
C
C Call the NR 2D interpolator, polynomial or bicubic spline
C
      IF (DOPOLY) THEN
         CALL POLIN2 (XBUF, YBUF, BUF, M2, M2, PX, PY, V, DV)
      ELSE
         CALL SPLIE2 (XBUF, YBUF, BUF, M2, M2, F2)
         CALL SPLIN2 (XBUF, YBUF, BUF, F2, M2, M2, PX, PY, V)
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
