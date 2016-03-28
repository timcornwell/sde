C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixcentr.f	1.1	 5/1/95
C
      SUBROUTINE PIXCENTR (IN, NX, NY, CX, CY)
C
CD Get the centroid of a 2-D array
C
C	IN	REAL(*)	input	Array to be corrected
C	NX	INT	input	Length of x-axis
C	NY	INT	input	Length of y-axis
C	CX	REAL	out	Centroid of IN
C	CY	REAL	out	Centroid of IN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	May 1 1995
C
C--------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY
      REAL		IN(NX, NY), CX, CY
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXCENTR')
C
      INTEGER		IX, IY, NSUM
C=========================================================================
      IF (ERROR) GO TO 999
C
      CX = 0.0
      CY = 0.0
      NSUM = 0
      DO 20 IY = 1, NY
         DO 10 IX = 1, NX
            IF (IN(IX, IY) .GT. 0.0) THEN
               NSUM = NSUM + 1
               CX = CX + IX * IN(IX, IY)
               CY = CY + IY * IN(IX, IY)
            ENDIF
   10       CONTINUE
   20    CONTINUE
         IF (NSUM .EQ. 0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $           'All pixels are LE zero, can''t determine centroid')
            GOTO 999
         ENDIF
         CX = CX / FLOAT(NSUM)
         CY = CY / FLOAT(NSUM)
C
 999  CONTINUE
      END
