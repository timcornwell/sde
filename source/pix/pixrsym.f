C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrsym.f	1.1	 2/21/91
C
      SUBROUTINE PIXRSYM (NX, NY, CX, CY, IN, SYM, ANTI)
C
CD Generates the symetric and antisymmetric parts of IN
C
C	NX	INT	input	number pix in X
C	NY	INT	input	number pix in Y
C	CX	REAL	input	center pixel, X
C	CY	REAL	input	center pixel, Y
C	IN	REAL	input	2-d array
C	SYM	REAL	out	2-d array, Sym
C	ANTI	REAL	out	2-d array, AntiSym
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 17 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX, NY
      REAL		CX, CY
      REAL		IN(NX, NY), SYM(NX, NY), ANTI(NX, NY)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRSYM')
      INTEGER		IX, IY, ICX, ICY, JX1, JX2, JY1, JY2
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      ICX = MIN (CX-1, NX-CX)
      ICY = MIN (CY-1, NY-CY)
C
      DO 200 IY = -ICY, ICY
         DO 100 IX = 0, ICX
            JX1 = CX - IX
            JX2 = CX + IX
            JY1 = CY - IY
            JY2 = CY + IY
            SYM(JX1, JY1) = (IN(JX1, JY1) + IN(JX2, JY2))/2.
            SYM(JX2, JY2) = SYM(JX1, JY1)
            ANTI(JX1, JY1) = (IN(JX1, JY1) - IN(JX2, JY2))/2.
            ANTI(JX2, JY2) = -ANTI(JX1, JY1)
 100     CONTINUE
 200  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
