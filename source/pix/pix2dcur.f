C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2dcur.f	1.4	 7/20/92
C
      SUBROUTINE PIX2DCUR (A, NX, NY, DELU, DELV, BCENX, BCENY, B)
C
CD Apply parabolic filter. Pixel level routine. 2-D complex only.
C
C	A	CMPX(*)	input	Image
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	DELU	REAL	input	Cellsize in U
C	DELV	REAL	input	Cellsize in V
C	BCENX	INT	input	Center of taper in X
C	BCENY	INT	input	Center of taper in Y
C	B	CMPX(*)	input	Tapered image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, BCENX, BCENY
      REAL		DELU, DELV
      COMPLEX		A(NX,*), B(NX,*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DCUR')
C
      INTEGER		IX, IY
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
      REAL		U, V, R, FACTU, FACTV, S2R
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      S2R = PI / (180.0*3600.0)
      FACTU = 2.0 * PI * DELU * S2R
      FACTV = 2.0 * PI * DELV * S2R
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
             U =   FACTU*FLOAT(IX-BCENX)
             V =   FACTV*FLOAT(IY-BCENY)
             R = U**2 + V**2
             B(IX,IY) = A(IX,IY) * R
  1      CONTINUE
  2   CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
