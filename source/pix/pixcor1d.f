C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixcor1d.f	1.3    11/7/90
C
      SUBROUTINE PIXCOR1D (IN, NX, CF, CFLEN, MODE, OUT, CENX)
C
CD Correct array for a given function
C
C	IN	REAL(*)	input	Array to be corrected
C	NX	INT	input	Length of x-axis
C	CF	REAL(*)	input	Correction array
C	CFLEN	INT	input	Length of correction array: this 
C				corresponds to the edge of IN
C	MODE	CH*(*)	input	APPLY|CORRECT
C	OUT	REAL(*)	output	Corrected array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODE
      INTEGER		NX, CFLEN
      REAL		IN(NX), OUT(NX), CF(*), CENX
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXCOR1D')
C
      REAL		SCALEX
      INTEGER		IX, ICFX
C=========================================================================
      IF (ERROR) GO TO 999
C
      SCALEX = (CFLEN-1) * 2.0 / FLOAT (NX)
C
      IF (MODE(1:5).EQ.'APPLY') THEN
         DO 10 IX = 1, NX
            ICFX = 1 + ABS(NINT (SCALEX*(FLOAT(IX) - CENX)))
            OUT (IX) = CF(ICFX) * IN (IX)
   10    CONTINUE
      ELSE IF (MODE(1:7).EQ.'CORRECT') THEN
         DO 20 IX = 1, NX
            ICFX = 1 + ABS(NINT (SCALEX*(FLOAT(IX) - CENX)))
            OUT (IX) = IN(IX) / CF(ICFX)
   20    CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unknown mode')
      END IF
C
  999 CONTINUE
      END
