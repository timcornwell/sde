C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixpb2d.f	1.3    11/7/90
C
      SUBROUTINE PIXPB2D (IN, NX, NY, CX, CY, BMX, BMY, BLANK, 
     1   MODE, NPB, PBARR, RADMAX, OUT)
C
CD Primary beam correction via look up table
C
C	IN	REAL(*)	input	Array to be corrected
C	NX	INT	input	Length of x-axis
C	NY	INT	input	Length of y-axis
C	CX	INT	input	Center of pb
C	BMX	REAL	input	Scaling for X
C	BLANK	REAL	input	Value of blank pixel
C	MODE	CH*(*)	input	'APPLY'|'CORRECT'
C	NPB	INT	input	Length of array for primary beam
C	PBARR	REAL	input	PB array
C	RADMAX	REAL	input	Maximum radius allowed
C	OUT	REAL(*)	output	Corrected array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NPB
      REAL		PBARR(*), RADMAX
      REAL		IN(NX, NY), OUT(NX, NY), CX, CY, BMX, BMY, BLANK
      CHARACTER*(*)	MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXPB2D')
C
      REAL		TAPER, YSQ, RAD, RSCL
      INTEGER		IX, IY, IPB
C=========================================================================
      IF (ERROR) GO TO 999
C
      RSCL = FLOAT(NPB-1) / RADMAX
C
      IF(SYSDEBUG) THEN
         WRITE(MESSAGE,1000) CX, CY
 1000    FORMAT ('PIXPB2D - pointing center at',2(1X,F8.3))
         CALL MSGPUT (MESSAGE, 'D')
      END IF
C
      IF (MODE(1:5).EQ.'APPLY') THEN
         DO 20 IY = 1, NY
            YSQ = BMY * (FLOAT(IY) - CY)**2
            DO 10 IX = 1, NX
               RAD = SQRT(YSQ + BMX*(FLOAT(IX)-CX)**2)
               IF (RAD.LE.RADMAX) THEN
                  TAPER = PBARR(1 + NINT(RSCL * RAD))
                  OUT (IX, IY) = TAPER * IN (IX, IY)
               ELSE
                  OUT (IX, IY) = BLANK
               END IF
   10       CONTINUE
   20    CONTINUE
      ELSE IF (MODE(1:7).EQ.'CORRECT') THEN
         DO 40 IY = 1, NY
            YSQ = BMY * (FLOAT(IY) - CY)**2
            DO 30 IX = 1, NX
               RAD = SQRT(YSQ + BMX*(FLOAT(IX)-CX)**2)
               IF (RAD.LE.RADMAX) THEN
                  TAPER = PBARR(1 + NINT(RSCL * RAD))
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
