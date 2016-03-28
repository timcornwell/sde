C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixcolla.f	1.3    11/7/90
C
      SUBROUTINE PIXCOLLA (IM3D, NX, NY, NZ, IM2D)
C
CD Project a sphere onto the plane X-Y.
C
C
C	IM3D	REAL	input	Input 3D sphere
C	NX	INT	input	Axis info
C	IM2D	REAL	output	Output 2D plane
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NZ
      REAL		IM3D(NX, NY, *), IM2D(NX, *)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXCOLLA')
C
      INTEGER		IX, IY, IZ, I
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 20 IY = 1, NY
         DO 10 IX = 1, NX
            IM2D(IX, IY) = 0.0
            DO 5 IZ = 1, NZ
               IM2D(IX, IY) = IM2D(IX, IY) + IM3D(IX, IY, IZ) 
  5        CONTINUE
  10    CONTINUE
  20  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
