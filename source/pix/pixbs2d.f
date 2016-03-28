C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixbs2d.f	1.2    7/20/92
C
      SUBROUTINE PIXBS2D (IN, NX, NY, CX1, CY1, CX2, CY2, BMX, BMY, 
     $   BLANK, MODE, GAIN1, NPB1, PBARR1, RADMAX1, GAIN2, NPB2, 
     $   PBARR2, RADMAX2, OUT)
C
CD Primary beam correction via look up table: For BEAM SWITCHING
C
C	IN	REAL(*)	input	Array to be corrected
C	NX	INT	input	Length of x-axis
C	NY	INT	input	Length of y-axis
C	CX1	INT	input	Center of pb
C	CX2	INT	input	Center of beam switched pb
C	BMX	REAL	input	Scaling for X
C	BLANK	REAL	input	Value of blank pixel
C	MODE	CH*(*)	input	'APPLY'|'CORRECT'
C	GAIN1	REAL	input	Gain for PB1
C	NPB1	INT	input	Length of array for primary beam
C	PBARR1	REAL	input	PB array
C	RADMAX1	REAL	input	Maximum radius allowed
C	GAIN2	REAL	input	Gain for PB2
C	NPB2	INT	input	Length of array for pswitched rimary beam
C	PBARR2	REAL	input	PB array, switched beam
C	RADMAX2	REAL	input	Maximum radius allowed, switched beam
C	OUT	REAL(*)	output	Corrected array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NPB1, NPB2
      REAL		PBARR1(*), RADMAX1, PBARR2(*), RADMAX2
      REAL		IN(NX, NY), OUT(NX, NY), CX1, CY1, CX2, CY2,
     $   		BMX, BMY, BLANK, GAIN1, GAIN2
      CHARACTER*(*)	MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXBS2D')
C
      REAL		TAPER1, TAPER2, YSQ1, YSQ2, RAD1, RAD2, RSCL1, 
     $   		RSCL2, TAPER
      INTEGER		IX, IY
C=========================================================================
      IF (ERROR) GO TO 999
C
      RSCL1 = FLOAT(NPB1-1) / RADMAX1
      RSCL2 = FLOAT(NPB2-1) / RADMAX2
C
      IF(SYSDEBUG) THEN
         WRITE(MESSAGE,1000) CX1, CY1
 1000    FORMAT ('PIXBS2D - pointing center at',2(1X,F8.3))
         CALL MSGPUT (MESSAGE, 'D')
         WRITE(MESSAGE,1001) CX2, CY2
 1001    FORMAT ('PIXBS2D - switched pointing at',2(1X,F8.3))
         CALL MSGPUT (MESSAGE, 'D')
      END IF
C
      IF (MODE(1:5).EQ.'APPLY') THEN
         DO 20 IY = 1, NY
            YSQ1 = BMY * (FLOAT(IY) - CY1)**2
            YSQ2 = BMY * (FLOAT(IY) - CY2)**2
            DO 10 IX = 1, NX
               RAD1 = SQRT(YSQ1 + BMX*(FLOAT(IX)-CX1)**2)
               RAD2 = SQRT(YSQ2 + BMX*(FLOAT(IX)-CX2)**2)
               TAPER1 = 0.
               TAPER2 = 0.
               IF (RAD1.LE.RADMAX1) THEN
                  TAPER1 = PBARR1(1 + NINT(RSCL1 * RAD1))
               ENDIF
               IF (RAD2.LE.RADMAX2) THEN
                  TAPER2 = PBARR2(1 + NINT(RSCL2 * RAD2))
               ENDIF
               IF (RAD1.LE.RADMAX1 .OR. RAD2.LE.RADMAX2) THEN
                  OUT (IX, IY) = GAIN1 * TAPER1 * IN (IX, IY) +
     $                           GAIN2 * TAPER2 * IN (IX, IY)
               ELSE
                  OUT (IX, IY) = BLANK
               END IF
   10       CONTINUE
   20    CONTINUE
      ELSE IF (MODE(1:7).EQ.'CORRECT') THEN
         DO 40 IY = 1, NY
            YSQ1 = BMY * (FLOAT(IY) - CY1)**2
            YSQ2 = BMY * (FLOAT(IY) - CY2)**2
            DO 30 IX = 1, NX
               RAD1 = SQRT(YSQ1 + BMX*(FLOAT(IX)-CX1)**2)
               RAD2 = SQRT(YSQ2 + BMX*(FLOAT(IX)-CX2)**2)
               TAPER1 = 0.
               TAPER2 = 0.
               TAPER  = 0.
               IF (RAD1.LE.RADMAX1) THEN
                  TAPER1 = PBARR1(1 + NINT(RSCL1 * RAD1))
               ENDIF
               IF (RAD2.LE.RADMAX2) THEN
                  TAPER2 = PBARR2(1 + NINT(RSCL2 * RAD2))
               ENDIF
               TAPER = GAIN1 * TAPER1 + GAIN2 * TAPER2
               IF (TAPER .NE. 0.0) THEN
                  OUT (IX, IY) = IN (IX, IY) / TAPER
               ELSE
                  OUT (IX, IY) = BLANK
               END IF
   30       CONTINUE
   40    CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unknown mode')
         GO TO 999
      END IF
C
 999  CONTINUE
      END
