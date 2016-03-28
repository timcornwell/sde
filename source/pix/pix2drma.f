C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drma.f	1.4    7/16/92
C
      SUBROUTINE PIX2DRMA (AL, FL, FN, XL, YL, BL, NL, A, NX, NY)
C
CD Make image from list of pixels. Pixel level routine. 2-D real only.
C
C
C	AL	REAL(*)	output	List of values
C	FL	INT(*)	input	List of field number
C	FN	INT	input	number of this field
C	XL, YL	INT(*)	output	X,Y lists
C	BL	INT	input	Eleement to start with
C	NL	INT	input	Maximum number of elements in list
C	A	REAL(*)	input	Name of Dirty image
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added field numbers
C				T.J. Cornwell	July 16 1992
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NL, BL, XL(*), YL(*), FL(*), FN
      REAL		A(NX,*), AL(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DRMA')
C
      INTEGER		IX, IY, I
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 30 I = BL, NL
         IX = XL(I)
         IY = YL(I)
         IF ((IX.GE.1).AND.(IX.LE.NX).AND.(IY.GE.1).AND.(IY.LE.NY)
     &      .AND.(FL(I).EQ.FN)) A(IX, IY) = A(IX, IY) + AL(I)
  30  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
