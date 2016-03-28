C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drli.f	1.4    6/3/92
C
      SUBROUTINE PIX2DRLI (A, NX, NY, SLIM, AL, XL, YL, NL, ANL)
C
CD Make list of pixels > SLIM. Pixel level routine. 2-D real only.
C
C
C	A	REAL(*)	input	Name of Dirty image
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	SLIM	REAL	input	Flux cutoff
C	AL	REAL(*)	output	List of values
C	XL, YL	INT(*)	output	X,Y lists
C	NL	INT	input	Maximum number of elements in list
C	ANL	INT	input	Actual number of elements in list
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added test to respect the maximum element limit.
C				D.S.Briggs	June 2 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NL, ANL, XL(*), YL(*)
      REAL		A(NX,*), AL(*)
      REAL		SLIM
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DRLI')
C
      INTEGER		IX, IY
C=====================================================================
      ANL = 0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Load pixels into list
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
            IF (ABS(A(IX,IY)).GT.SLIM) THEN
               ANL = ANL + 1
               AL(ANL) = A(IX, IY)
               XL(ANL) = IX
               YL(ANL) = IY
               IF (ANL.GE.NL) GO TO 990
            END IF
  1      CONTINUE
  2   CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END

