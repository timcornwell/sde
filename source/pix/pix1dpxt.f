C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C %W% %G%
C
      SUBROUTINE PIX1DPXT (A, NX, INNER, OUTER, POWER, BCENX, B)
C
CD Taper an image with a power law. Pixel level routine. 1-D complex only.
C
C	A	CMPX(*)	input	Image
C	NX	INT	input	Size of X-axis of A
C	INNER	REAL	input	Inner Scale (pixels)
C	OUTER	REAL	input	Outer Scale (pixels)
C	POWER	REAL	input	Power law index
C	BCENX	INT	input	Center of taper in X
C	B	CMPX(*)	input	Tapered image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, BCENX
      COMPLEX		A(*), B(*)
      REAL		INNER, OUTER, POWER
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX1DPXT')
C
      INTEGER		IX
      REAL		R
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF(POWER.EQ.0.0) GO TO 999
C
      IF(POWER.GT.0.0) THEN
         DO 1 IX = 1, NX
            R = ABS(FLOAT(IX-BCENX))
            IF (R.GT.INNER) THEN
               IF(R.LT.OUTER) THEN
                  B(IX) = A(IX)*(R/INNER)**POWER
               ELSE
                  B(IX) = 0.0
               ENDIF
            ENDIF
  1      CONTINUE
      ELSE
         DO 3 IX = 1, NX
            R = ABS(FLOAT(IX-BCENX))
            IF (R.GT.INNER) THEN
               IF(R.LT.OUTER) THEN
                  B(IX) = A(IX)/(R/INNER)**POWER
               ELSE
                  B(IX) = 0.0
               ENDIF
            ENDIF
 3       CONTINUE
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
