C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2dgrt.f	1.2    10/21/93
C
      SUBROUTINE PIX2DGRT (A, NX, NY, BMAJ, BMIN, BPA, BCENX, BCENY, B)
C
CD Taper an image. Pixel level routine. 2-D real only.
C
C	A	REAL(*)	input	Image
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	BMAJ	REAL	input	Major axis
C	BMIN	REAL	input	Minor axis
C	BPA	REAL	input	Position
C	BCENX	INT	input	Center of taper in X
C	BCENY	INT	input	Center of taper in Y
C	B	REAL(*)	input	Tapered image
C Audit trail:
C	Cloned from PIX2DGXT
C				D.S.Briggs	Mar 1993
C	Added inverse taper
C				D.S.Briggs	Oct 20 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, BCENX, BCENY
      REAL		A(NX,NY), B(NX,NY)
      REAL		BMAJ, BMIN, BPA
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DGRT')
C
      INTEGER		IX, IY
      REAL	PI
      PARAMETER	(PI=3.14159265359)
      REAL		U, V, R, FACT, COSPA, SINPA, RBMAJ, RBMIN
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      COSPA = COS(PI*BPA/180.0)
      SINPA = SIN(PI*BPA/180.0)
C
      IF (BMAJ.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'BMAJ zero')
         GO TO 999
      END IF
      IF (BMIN.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'BMIN zero')
         GO TO 999
      END IF
C
      FACT = 4.0*LOG(2.0)
      RBMAJ = SIGN(FACT/BMAJ**2,BMAJ)
      RBMIN = SIGN(FACT/BMIN**2,BMIN)
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
             U =   COSPA * FLOAT(IX-BCENX) + SINPA * FLOAT(IY-BCENY)
             V = - SINPA * FLOAT(IX-BCENX) + COSPA * FLOAT(IY-BCENY)
             R = RBMAJ*U**2 + RBMIN*V**2
             B(IX,IY) = A(IX,IY) * EXP(-R)
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
