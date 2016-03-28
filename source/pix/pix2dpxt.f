C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2dpxt.f	1.3    11/7/90
C
      SUBROUTINE PIX2DPXT (A, NX, NY, INNER, OUTER, POWER, BCENX,
     $   BCENY, B)
C
CD Taper an image with a power law. Pixel level routine. 2-D complex only.
C
C	A	CMPX(*)	input	Image
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	INNER	REAL	input	Inner Scale (pixels)
C	OUTER	REAL	input	Outer Scale (pixels)
C	POWER	REAL	input	Power law index
C	BCENX	INT	input	Center of taper in X
C	BCENY	INT	input	Center of taper in Y
C	B	CMPX(*)	input	Tapered image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, BCENX, BCENY
      COMPLEX		A(NX,*), B(NX,*)
      REAL		INNER, OUTER, POWER
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DPXT')
C
      INTEGER		IX, IY
      REAL		R, INNERSQ, OUTERSQ, POWERD2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      INNERSQ = INNER**2
      OUTERSQ = OUTER**2
      IF(POWER.EQ.0.0) GO TO 999
      POWERD2 = ABS(POWER/2.0)
C
      IF(POWER.GT.0.0) THEN
         DO 2 IY = 1, NY
            DO 1 IX = 1, NX
               R = FLOAT(IX-BCENX)**2 + FLOAT(IY-BCENY)**2
               IF (R.GT.INNERSQ) THEN
                  IF(R.LT.OUTERSQ) THEN
                     B(IX,IY) = A(IX,IY)*(R/INNERSQ)**POWERD2
                  ELSE
                     B(IX,IY) = 0.0
                  ENDIF
               ENDIF
  1         CONTINUE
  2      CONTINUE
      ELSE
         DO 4 IY = 1, NY
            DO 3 IX = 1, NX
               R = FLOAT(IX-BCENX)**2 + FLOAT(IY-BCENY)**2
               IF (R.GT.INNERSQ) THEN
                  IF(R.LT.OUTERSQ) THEN
                     B(IX,IY) = A(IX,IY)/(R/INNERSQ)**POWERD2
                  ELSE
                     B(IX,IY) = 0.0
                  ENDIF
               ENDIF
 3          CONTINUE
 4       CONTINUE
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
