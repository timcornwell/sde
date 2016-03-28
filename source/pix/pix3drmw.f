C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix3drmw.f	1.1   5/4/91
C
      SUBROUTINE PIX3DRMW (AL, XL, YL, ZL, BL, NL, A, NX, NY, NZ, TFLUX)
C
CD Multiply list of pixels. Pixel level routine. 3-D real only.
C
C
C	AL	REAL(*)	output	List of values
C	XL, YL	INT(*)	output	X,Y,Z lists
C	BL	INT	input	Element to start with
C	NL	INT	input	Maximum number of elements in list
C	A	REAL(*)	input	Name of window image
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	TFLUX	REAL	output	Total flux in components
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NZ, NL, BL, XL(*), YL(*), ZL(*)
      REAL		A(NX,NY,*), AL(*), TFLUX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX3DRMW')
C
      INTEGER		IX, IY, IZ, I
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I = BL, NL
         IX = XL(I)
         IY = YL(I)
         IZ = ZL(I)
         IF ((IX.GE.1).AND.(IX.LE.NX).AND.(IY.GE.1).AND.(IY.LE.NY))
     $      AL(I) = AL(I) * A(IX,IY,I)
         TFLUX = TFLUX + AL(I)
  10  CONTINUE
C
 999  CONTINUE
      END
