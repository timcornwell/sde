C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix1dgxt.f	1.2    10/21/93
C
      SUBROUTINE PIX1DGXT (A, NX, BMAJ, BCENX, B)
C
CD Taper an image. Pixel level routine. 1-D complex only.
C
C	A	CMPX(*)	input	Image
C	NX	INT	input	Size of X-axis of A
C	BMAJ	REAL	input	Major axis
C	BCENX	INT	input	Center of taper in X
C	B	CMPX(*)	input	Tapered image
C Audit trail:
C	Original version: Cloned from PIX2DGXT
C	and successive lines
C				D.S.Briggs	Oct 21 1992
C	Added inverse taper
C				D.S.Briggs	Oct 20 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, BCENX
      COMPLEX		A(NX), B(NX)
      REAL		BMAJ
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DGXT')
C
      INTEGER		IX
      REAL	PI
      PARAMETER	(PI=3.14159265358979323846)
      REAL		U, R, FACT, RBMAJ
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (BMAJ.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'BMAJ zero')
         GO TO 999
      END IF
C
      FACT = 4.0*LOG(2.0)
      RBMAJ = SIGN(FACT/BMAJ**2,BMAJ)
C
      DO 1 IX = 1, NX
         U = FLOAT(IX-BCENX)
         R = RBMAJ*U**2
         B(IX) = A(IX) * EXP(-R)
 1    CONTINUE
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
