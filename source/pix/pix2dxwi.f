C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2dxwi.f	1.3    11/7/90
C
      SUBROUTINE PIX2DXWI (A, B, NX, NY, MIND, C)
C
CD Taper an image with inverse amplitudes of another. 
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
      COMPLEX		A(NX,*), B(NX,*), C(NX,*)
      REAL		MIND
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DXWI')
C
      INTEGER		IX, IY
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C
      IF (MIND.LE.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'MIND <= zero')
         GO TO 999
      END IF
C
      DO 20 IY = 1, NY
         DO 10 IX = 1, NX
            C(IX,IY) = A(IX,IY) * CONJG(B(IX,IY))/
     1         (ABS(B(IX,IY))**2+MIND**2)
  10     CONTINUE
  20  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE
      END IF
C
 999  CONTINUE
      END
