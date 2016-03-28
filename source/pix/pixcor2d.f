C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixcor2d.f	1.3    11/7/90
C
      SUBROUTINE PIXCOR2D (IN, NX, NY, CF, CFLEN, MODE, OUT, CENX,
     $   CENY)
C
CD Correct array for a given function
C
C	IN	REAL(*)	input	Array to be corrected
C	NX	INT	input	Length of x-axis
C	NY	INT	input	Length of y-axis
C	CF	REAL(*)	input	Correction array
C	CFLEN	INT	input	Length of correction array: this 
C				corresponds to the edge of IN
C	MODE	CH*(*)	input	APPLY|CORRECT
C	OUT	REAL(*)	output	Corrected array
C	CENX	REAL	input	Reference pixel
C	CENY	REAL	input	Reference pixel
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODE
      INTEGER		NX, NY, CFLEN
      REAL		IN(NX, *), OUT(NX, *), CF(*), CENX, CENY
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXCOR2D')
C
      REAL		SCALEX, SCALEY
      INTEGER		IX, IY, ICFX, ICFY
      INTEGER		FNX, FNY, FFTPWR2
C=========================================================================
      IF (ERROR) GO TO 999
C
      FNX = FFTPWR2(NX)
      FNY = FFTPWR2(NY)
C
      SCALEX = (CFLEN-1) * 2.0 / FLOAT (FNX)
      SCALEY = (CFLEN-1) * 2.0 / FLOAT (FNY)
C
      IF (MODE(1:5).EQ.'APPLY') THEN
         DO 20 IY = 1, NY
            ICFY = 1 + ABS(NINT (SCALEY*(FLOAT(IY) - CENY)))
            DO 10 IX = 1, NX
               ICFX = 1 + ABS(NINT (SCALEX*(FLOAT(IX) - CENX)))
               OUT (IX, IY) = CF(ICFX) * CF(ICFY) * IN (IX, IY)
   10       CONTINUE
   20    CONTINUE
      ELSE IF (MODE(1:7).EQ.'CORRECT') THEN
         DO 40 IY = 1, NY
            ICFY = 1 + ABS(NINT (SCALEY*(FLOAT(IY) - CENY)))
            DO 30 IX = 1, NX
               ICFX = 1 + ABS(NINT (SCALEX*(FLOAT(IX) - CENX)))
               OUT (IX, IY) = IN(IX, IY)/(CF(ICFX) * CF(ICFY))
   30       CONTINUE
   40    CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unknown mode')
      END IF
C
 999  CONTINUE
      END
