C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix3drli.f	1.3    11/7/90
C
      SUBROUTINE PIX3DRLI (A, NX, NY, NZ, SLIM, AL, XL, YL, ZL, 
     1   NL, ANL)
C
CD Make list of pixels > SLIM. Pixel level routine. 3-D real only.
C This version does not check that ANL does not exceed NL so be very
C careful.
C
C
C	A	REAL(*)	input	Name of Dirty image
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	NZ	INT	input	Size of Z-axis of A
C	SLIM	REAL	input	Flux cutoff
C	AL	REAL(*)	output	List of values
C	XL, YL, ZL	INT(*)	output	X,Y,Z lists
C	NL	INT	input	Maximum number of elements in list
C	ANL	INT	input	Actual number of elements in list
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NZ, NL, ANL, XL(*), YL(*), ZL(*)
      REAL		A(NX,NY,*), AL(*)
      REAL		SLIM
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX3DRLI')
C
      INTEGER		IX, IY, IZ
C=====================================================================
      ANL = 0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Load pixels into list
C
      DO 3 IZ = 1, NZ
         DO 2 IY = 1, NY
            DO 1 IX = 1, NX
               IF (ABS(A(IX, IY, IZ)).GT.SLIM) THEN
                  ANL = ANL + 1
                  AL(ANL) = A(IX, IY, IZ)
                  XL(ANL) = IX
                  YL(ANL) = IY
                  ZL(ANL) = IZ
               END IF
  1         CONTINUE
  2      CONTINUE
  3   CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
