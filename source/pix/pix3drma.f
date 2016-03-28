C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix3drma.f	1.4    7/16/92
C
      SUBROUTINE PIX3DRMA (AL, FL, FN, XL, YL, ZL, BL, NL, A, NX,
     $   NY, NZ)
C
CD Make image from list of pixels. Pixel level routine. 3-D real only.
C
C
C	AL	REAL(*)	output	List of values
C	XL, YL, ZL	INT(*)	input	X,Y,Z lists
C	BL	INT	input	Eleement to start with
C	NL	INT	input	Maximum number of elements in list
C	A	REAL(*)	input	Name of Dirty image
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	NZ	INT	input	Size of Z-axis of A
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added field numbers
C				T.J. Cornwell	July 16 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NZ, NL, BL, XL(*), YL(*), ZL(*),
     $			FL(*), FN
      REAL		A(NX,NY,*), AL(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX3DRMA')
C
      INTEGER		IX, IY, IZ, I
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 30 I = BL, NL
         IX = XL(I)
         IY = YL(I)
         IZ = ZL(I)
         IF ((IX.GE.1).AND.(IX.LE.NX).AND.(IY.GE.1).AND.(IY.LE.NY)
     1      .AND.(IZ.GE.1).AND.(IZ.LE.NZ).AND.(FL(I).EQ.FN))
     1      A(IX, IY, IZ) = A(IX, IY, IZ) + AL(I)
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
