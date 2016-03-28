C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixpb2dv.f	1.3    11/7/90
C
      SUBROUTINE PIXPB2DV (IN, NX, NY, CX, CY, BMX, BMY, BLANK, 
     1   MODE, OUT)
C
CD 2DVLA primary beam for VLA
C
C	IN	REAL(*)	input	Array to be corrected
C	NX	INT	input	Length of x-axis
C	NY	INT	input	Length of y-axis
C	CX	INT	input	Center of pb
C	BMX	REAL	input	Scaling for X
C	BLANK	REAL	input	Value of blank pixel
C	MODE	CH*(*)	input	'APPLY'|'CORRECT'
C	OUT	REAL(*)	output	Corrected array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C       Truncate at zero rather than at 7% (only when applying)
C				T.J.Cornwell	Sept 29 1990
C
C--------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY
      REAL		IN(NX, *), OUT(NX, *), CX, CY, BMX, BMY, BLANK
      CHARACTER*(*)	MODE
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXPB2DV')
C
      REAL		TAPER, YSQ, RADSQ
      INTEGER		IX, IY
C=========================================================================
      IF (ERROR) GO TO 999
C
      IF (MODE.EQ.'APPLY') THEN
         DO 20 IY = 1, NY
            YSQ = BMY * (FLOAT(IY) - CY)**2
            DO 10 IX = 1, NX
               RADSQ = YSQ + BMX*(FLOAT(IX)-CX)**2
               TAPER =  1.0 - 0.1329074E-2*RADSQ + 0.6920305E-6*RADSQ**2
     *                      - 0.1434144E-9*RADSQ**3
               IF (RADSQ.LE.1885.0) THEN
                  OUT (IX, IY) = TAPER * IN (IX, IY)
               ELSE
                  OUT (IX, IY) = BLANK
               END IF
   10       CONTINUE
   20    CONTINUE
      ELSE
         DO 40 IY = 1, NY
            YSQ = BMY * (FLOAT(IY) - CY)**2
            DO 30 IX = 1, NX
               RADSQ = YSQ + BMX*(FLOAT(IX)-CX)**2
               TAPER =  1.0 - 0.1329074E-2*RADSQ + 0.6920305E-6*RADSQ**2
     *                      - 0.1434144E-9*RADSQ**3
               IF (TAPER.GE.0.07) THEN
                  OUT (IX, IY) = IN (IX, IY) / TAPER
               ELSE
                  OUT (IX, IY) = BLANK
               END IF
   30       CONTINUE
   40    CONTINUE
      END IF
C
 999  CONTINUE
      END
