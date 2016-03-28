C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drmw.f	1.1    3/14/91
C
      SUBROUTINE PIX2DRMW (AL, XL, YL, BL, NL, A, NX, NY, TFLUX)
C
CD Multiply list of pixels. Pixel level routine. 2-D real only.
C
C
C	AL	REAL(*)	output	List of values
C	XL, YL	INT(*)	output	X,Y lists
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
      INTEGER		NX, NY, NL, BL, XL(*), YL(*)
      REAL		A(NX,*), AL(*), TFLUX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DRMW')
C
      INTEGER		IX, IY, I
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I = BL, NL
         IX = XL(I)
         IY = YL(I)
         IF ((IX.GE.1).AND.(IX.LE.NX).AND.(IY.GE.1).AND.(IY.LE.NY))
     $      AL(I) = AL(I) * A(IX,IY)
         TFLUX = TFLUX + AL(I)
  10  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
