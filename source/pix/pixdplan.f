C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdplan.f	1.1	 5/13/93
C
      SUBROUTINE PIXDPLAN (A1, A2, NX, NY, P1, P2, P3)
C
CD Subtracts a PLANE off of an image
C  The plane is defined by P1, P2, P3, each the X, Y, Z pixel coords of a point
C
C	A1	REAL	input	Real array
C	A2	REAL	out	Real array
C	P1	REAL(3)	inp	Real X, Y, Z of point 1 on plane
C	P2	REAL(3)	inp	Real X, Y, Z of point 2 on plane
C	P3	REAL(3)	inp	Real X, Y, Z of point 3 on plane
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	April 9 1993
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY
      REAL		A1(NX, *), A2(NX, *), P1(3), P2(3), P3(3)
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'ARRDPLAN')
C
      INTEGER		IX, IY
      REAL		VA(3), VB(3), VN(3), Z
C=====================================================================
C
      IF (ERROR) GOTO 999
C
C Equation of line: n.r = 0  where n is normal to the plane, 
C r = [x - P1(1), y - P1(2), z - P1(3)]
C n is found by the cross product of vector P2-P1 x P3-P1 (VN = VA x VB)
C
      VA(1) = P2(1) - P1(1)
      VA(2) = P2(2) - P1(2)
      VA(3) = P2(3) - P1(3)
      VB(1) = P3(1) - P1(1)
      VB(2) = P3(2) - P1(2)
      VB(3) = P3(3) - P1(3)
C
      VN(1) = VA(2)*VB(3) - VA(3)*VB(2)
      VN(2) = VA(3)*VB(1) - VA(1)*VB(3)
      VN(3) = VA(1)*VB(2) - VA(2)*VB(1)
C
      IF (VN(3) .EQ. 0.0) THEN 
         CALL MSGPUT ('Plane is INDEPENDENT of Z', 'W')
         DO 200 IY = 1, NY
            DO 100 IX = 1, NX
               Z = P1(3)
 100       CONTINUE
 200    CONTINUE
      ELSE
         DO 1200 IY = 1, NY
            DO 1100 IX = 1, NX
               Z = P1(3) - (VN(1)*(IX-P1(1)) + VN(2)*(IY-P1(2)))/ VN(3)
               A2(IX, IY) = A1(IX, IY) - Z
 1100       CONTINUE
 1200    CONTINUE
      ENDIF

 999  CONTINUE
      END
