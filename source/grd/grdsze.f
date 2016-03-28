C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdsze.f	1.3    11/7/90
C
      REAL FUNCTION GRDSZE (NU)
C
CD Find the optimum gridding function described by Sze Tan in his
C thesis. This is case R = 3, X0 = 0.45. It is very good to within
C 10% of the edge of the image.
C
C
C	NU	REAL	input	Distance from center
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	NU
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GRDSZE')
C
      REAL		DELNUSQ, NUEND
      REAL		TOP, BOT
      INTEGER		K, PART
      INTEGER		NP, NQ
      PARAMETER		(NP = 4)
      PARAMETER		(NQ = 2)
      REAL		P(0:NP,3), Q(0:NQ,3)
      DATA	P	/6.0527199E-1, 3.3587534E-1, -2.1497621E-1,
     1			3.8904394E-2, -5.3295409E-3,
     2			1.1049142E-1, -3.4967472E-2, -6.6916751E-4,
     3			1.1027297E-3, -2.5684433E-4,
     4			6.0855041E-4, -1.8814394E-3, 6.5014897E-4,
     5			-1.9612551E-4, 9.4834730E-7/
C
      DATA	Q	/1.000000000, 1.0711361, 8.1322567E-2,
     1			1.000000000, 3.1491945E-1, 2.3050743E-2,
     2			1.000000000, 1.4856126E-1, 8.5105642E-3/
C=======================================================================
      GRDSZE = 0.0
      IF (ERROR) GO TO 999
C
      IF ((NU.GE.0.0).AND.(NU.LT.1.0)) THEN
         PART = 1
         NUEND = 1.0
      ELSEIF ((NU.GE.1.0).AND.(NU.LE.2.00)) THEN
         PART = 2
         NUEND = 2.00
      ELSEIF ((NU.GE.2.0).AND.(NU.LE.3.00)) THEN
         PART = 3
         NUEND = 3.00
      ELSE 
         GRDSZE = 0.0
         GO TO 999
      END IF
C
      TOP = P(0, PART)
      DELNUSQ = NU**2 - NUEND**2
      DO 10 K = 1, NP
         TOP = TOP + P(K,PART) * DELNUSQ ** K
  10  CONTINUE
      BOT = Q(0,PART)
      DO 20 K = 1, NQ
         BOT = BOT + Q(K,PART) * DELNUSQ ** K
  20  CONTINUE
      IF (BOT.NE.0.0) THEN
         GRDSZE = TOP/BOT
      ELSE
         GRDSZE = 0.0
      END IF
C
 999  CONTINUE
      END
