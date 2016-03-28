C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixcor3d.f	1.3    11/7/90
C
      SUBROUTINE PIXCOR3D (IN, NX, NY, NZ, CF, CFLEN, MODE, OUT,
     $   CENX, CENY, CENZ)
C
CD Correct array for a given function
C
C	IN	REAL(*)	input	Array to be corrected
C	NX	INT	input	Length of x-axis
C	NY	INT	input	Length of y-axis
C	NZ	INT	input	Length of z-axis
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
      INTEGER		NX, NY, NZ, CFLEN
      REAL		IN(NX, NY, *), OUT(NX, NY, *), CF(*),
     $			CENX, CENY, CENZ
      CHARACTER*(*)	MODE
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXCOR3D')
C
      REAL		SCALEX, SCALEY, SCALEZ, CFYZ
      INTEGER		IX, IY, IZ, ICFX, ICFY, ICFZ
C=========================================================================
      IF (ERROR) GO TO 999
C
      SCALEX = (CFLEN-1) * 2.0 / FLOAT (NX)
      SCALEY = (CFLEN-1) * 2.0 / FLOAT (NY)
      SCALEZ = (CFLEN-1) * 2.0 / FLOAT (NZ)
C
      IF (MODE(1:5).EQ.'APPLY') THEN
         DO 30 IZ = 1, NZ
            ICFZ = 1 + ABS(NINT (SCALEZ*(FLOAT(IZ) - CENZ)))
            DO 20 IY = 1, NY
               ICFY = 1 + ABS(NINT (SCALEY*(FLOAT(IY) - CENY)))
               CFYZ = CF(ICFY) * CF(ICFZ)
               DO 10 IX = 1, NX
                  ICFX = 1 + ABS(NINT (SCALEX*(FLOAT(IX) - CENX)))
                  OUT (IX, IY, IZ) = CF(ICFX) * CFYZ * IN (IX, IY, IZ)   
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
      ELSE IF (MODE(1:7).EQ.'CORRECT') THEN
         DO 60 IZ = 1, NZ
            ICFZ = 1 + ABS(NINT (SCALEZ*(FLOAT(IZ) - CENZ)))
            DO 50 IY = 1, NY
               ICFY = 1 + ABS(NINT (SCALEY*(FLOAT(IY) - CENY)))
               CFYZ = 1.0/(CF(ICFY) * CF(ICFZ))
               DO 40 IX = 1, NX
                  ICFX = 1 + ABS(NINT (SCALEX*(FLOAT(IX) - CENX)))
                  OUT (IX, IY, IZ) =  CFYZ * IN (IX, IY, IZ) / CF(ICFX) 
   40          CONTINUE
   50       CONTINUE
   60    CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unknown mode')
      END IF
C
 999  CONTINUE
      END
