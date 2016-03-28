C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpcan2.f	1.1	 5/1/95
C
      SUBROUTINE IMGPCAN2 (IN, NX, NY, CX, CY, 
     1   DX, DY, RAD1, RAD2)
C
CD Make an annulus about the pointing center
C
C	IN	REAL(*)	in/out	Array to be corrected
C	NX	INT	input	Length of x-axis
C	NY	INT	input	Length of y-axis
C       CX	REAL	input	Center of pb
C	DX	REAL	input	Cell Size, degrees
C	RAD1	REAL	input	Inner Radius, arcsec
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	April 30 1995
C
C--------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY
      REAL		RAD1, RAD2, DX, DY
      REAL		IN(NX, NY), CX, CY
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPCAN2')
C
      REAL		YSQ, RAD1SQ, RAD2SQ, R2
      INTEGER		IX, IY
C=========================================================================
      IF (ERROR) GO TO 999
C
      RAD1SQ = (RAD1/3600.0)**2
      RAD2SQ = (RAD2/3600.0)**2
C
      IF(SYSDEBUG) THEN
         WRITE(MESSAGE,1000) CX, CY
 1000    FORMAT ('PIXPB2D - pointing center at',2(1X,F8.3))
         CALL MSGPUT (MESSAGE, 'D')
      END IF
C
      DO 20 IY = 1, NY
         YSQ = ( DY * (FLOAT(IY) - CY))**2
         DO 10 IX = 1, NX
            R2 = YSQ + (DX* (FLOAT(IX)-CX))**2
            IF (R2 .GE. RAD1SQ .AND. R2 .LE. RAD2SQ) THEN
               IN (IX, IY) = 1.0
            ELSE
               IN (IX, IY) = 0.0
            END IF
 10      CONTINUE
 20   CONTINUE
C     
 999  CONTINUE
      END
