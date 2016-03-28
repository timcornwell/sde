C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2dxdt.f	1.4	 7/20/92
C
      SUBROUTINE PIX2DXDT (A, B, NX, NY, MIND, C)
C
C Taper an image with inverse amplitudes of another. 
C Pixel level routine. 2-D complex only.
C
C
C	A	CMPX(*)	input	Image
C	B	REAL(*)	input	Dividing amplitudes
C	NX	INT	input	Number of points in x dir
C	NY	INT	input	Number of points in y dir
C	MIND	REAL	input	Minimum denominator
C	C	CMPX(*)	input	Tapered image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
C
#include 		"stdinc.h"
C
      INTEGER		NX, NY
      COMPLEX		A(NX,*), C(NX,*)
      REAL		B(NX,*), MIND
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DXDT')
C
      INTEGER		IX, IY
      REAL		TAPER
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (MIND.LE.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'MIND <= zero')
         GO TO 999
      END IF
C
      DO 20 IY = 1, NY
         DO 10 IX = 1, NX
            IF(B(IX,IY).GT.MIND) THEN
               TAPER = 1./B(IX,IY)
            ELSE
               TAPER = 1.0
            END IF
            C(IX,IY) = A(IX,IY) * TAPER
  10     CONTINUE
  20  CONTINUE
C
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
