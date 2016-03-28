C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix3dxgt.f	1.4    10/21/93
C
      SUBROUTINE PIX3DXGT (A, NX, NY, NZ, BMAJ, BMIN, BPA, 
     1   BZ, BCENX, BCENY, BCENZ, B)
C
CD Taper an image. Pixel level routine. 3-D complex only.
C
C   BZ, BCENX, BCENY, BCENZ, B)
C
C	A	CMPX(*)	input	Image
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	BMAJ	REAL	input	Major axis
C	BMIN	REAL	input	Minor axis
C	BPA	REAL	input	Position
C	BCENX	INT	input	Center of taper in X
C	BCENY	INT	input	Center of taper in Y
C	B	CMPX(*)	input	Tapered image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added inverse taper
C				D.S.Briggs	Oct 20 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NZ, BCENX, BCENY, BCENZ
      COMPLEX		A(NX, NY, *), B(NX, NY, *)
      REAL		BMAJ, BMIN, BPA, BZ
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX3DXGT')
C
      INTEGER		IX, IY, IZ
      REAL	PI
      PARAMETER	(PI=3.14159274101257)

      REAL		U, V, W, R, FACT, COSPA, SINPA, RBMAJ, RBMIN,
     1			RBZ
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
      IF (BZ.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'BZ zero')
         GO TO 999
      END IF
C
      FACT = 4.0*LOG(2.0)
      RBMAJ = SIGN(FACT/BMAJ**2,BMAJ)
      RBMIN = SIGN(FACT/BMIN**2,BMIN)
      RBZ = SIGN(FACT/BZ**2,BZ)
C
      DO 3 IZ = 1, NZ
         DO 2 IY = 1, NY
            DO 1 IX = 1, NX
                U =   COSPA * FLOAT(IX-BCENX) + SINPA * FLOAT(IY-BCENY)
                V = - SINPA * FLOAT(IX-BCENX) + COSPA * FLOAT(IY-BCENY)
                W = FLOAT(IZ-BCENZ)
                R = RBMAJ*U**2 + RBMIN*V**2 + RBZ*W**2
                B(IX,IY,IZ) = A(IX,IY,IZ) * EXP(-R)
  1         CONTINUE
  2      CONTINUE
  3   CONTINUE
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
