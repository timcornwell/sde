C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlbess.f	1.3	 7/20/92
C
      REAL FUNCTION BESJ1(X)
C
C Make a J1 Bessel Function: From Numerical recipes.
C
C	X	REAL	input	Argument
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		X
      REAL 		AX,Z
      DOUBLE PRECISION	XX,Y,ANS,ANS1,ANS2
C-----------------------------------------------------------------------
      AX=ABS(X)
      IF (AX.LT.8.0) THEN
         Y=X*X
	 ANS1=X*(72362614232.0+Y*(-7895059235.0+Y*(242396853.1
     $      +Y*(-2972611.439+Y*(15704.48260+Y*(-30.16036606))))))
	 ANS2=144725228442.0+Y*(2300535178.0+Y*(18583304.74
     $      +Y*(99447.43394+Y*(376.9991397+Y*1.0))))
	 ANS=ANS1/ANS2
      ELSE
	Z=8.0/AX
	Y=Z*Z
	XX=AX-2.356194491
        ANS1=1.0+Y*(0.183105E-2+Y*(-0.3516396496E-4
     $     +Y*(0.2457520174E-5+Y*(-0.240337019E-6))))
        ANS2=0.04687499995+Y*(-0.2002690873E-3
     $     +Y*(0.8449199096E-5+Y*(-0.88228987E-6
     $     +Y*0.105787412E-6)))
        ANS=SQRT(0.636619772/AX)*(COS(XX)*ANS1-Z*SIN(XX)*ANS2)
        IF (X.LT.0.0) ANS = -ANS
      ENDIF
      BESJ1 = ANS
      END
      REAL FUNCTION BESJ0(X)
C
C Make a J0 Bessel Function: From Numerical recipes.
C
C	X	REAL	input	Argument
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	X
C
      REAL	AX, Z, XX
      REAL*8 Y,P1,P2,P3,P4,P5,Q1,Q2,Q3,Q4,Q5,R1,R2,R3,R4,R5,R6,
     *    S1,S2,S3,S4,S5,S6
      DATA P1,P2,P3,P4,P5/1.D0,-.1098628627D-2,.2734510407D-4,
     *    -.2073370639D-5,.2093887211D-6/, Q1,Q2,Q3,Q4,Q5/-.1562499995D-
     *1,
     *    .1430488765D-3,-.6911147651D-5,.7621095161D-6,-.934945152D-7/
      DATA R1,R2,R3,R4,R5,R6/57568490574.D0,-13362590354.D0,651619640.7D
     *0,
     *    -11214424.18D0,77392.33017D0,-184.9052456D0/,
     *    S1,S2,S3,S4,S5,S6/57568490411.D0,1029532985.D0,
     *    9494680.718D0,59272.64853D0,267.8532712D0,1.D0/
      IF(ABS(X).LT.8.)THEN
        Y=X**2
        BESJ0=(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6)))))
     *      /(S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
      ELSE
        AX=ABS(X)
        Z=8./AX
        Y=Z**2
        XX=AX-.785398164
        BESJ0=SQRT(.636619772/AX)*(COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y
     *      *P5))))-Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
      ENDIF
      RETURN
      END
