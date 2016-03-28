C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixsph.f	1.2    11/7/90
C
      SUBROUTINE PIXSPH (IM2D, NX, NY, NZ, IM3D, RX, RY, RZ, 
     1   CX, CY, CZ)
C
CD Expand into a sphere from the plane X-Y. The center of the sphere
C is at CX, CY, CZ pixels. Simple linear-interpolation is used, so
C this scheme is approximate. It is good only for low-dynamic range
C images such as noise.
C
C
C	IM2D	REAL	input	Input 2D plane
C	NX	INT	input	Axis info
C	IM3D	REAL	output	Output 3D sphere
C	RX	REAL	input	Radius of cube in X direction
C	CX,..	REAL	input	Center of sphere in IM3D
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
      REAL		RX, RY, RZ, CX, CY, CZ
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXSPH')
C
      INTEGER		IX, IY, IZ
      REAL		Z, X, PI, OSAMP
      INTEGER		LENSINC, LASTNZ, I
      PARAMETER		(LENSINC = 1024)
      REAL 		SINC(0:LENSINC)
      DATA		PI	/3.14159274101257/
      DATA		LASTNZ	/0/
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (NZ.NE.LASTNZ) THEN
         OSAMP = FLOAT(LENSINC)/FLOAT(NZ)
         SINC(0) = 1.0
         DO 4 IZ = 1, LENSINC
            X = PI * FLOAT(IZ) / OSAMP
            SINC(IZ) = SIN(X)/X
 4       CONTINUE
         LASTNZ = NZ
      END IF
C
      DO 3 IZ = 1, NZ
         DO 2 IY = 1, NY
            DO 1 IX = 1, NX
               IM3D(IX,IY,IZ) = 0.0
   1        CONTINUE
   2     CONTINUE
   3  CONTINUE
C
      DO 20 IY = 1, NY
         DO 10 IX = 1, NX
            Z = CZ + RZ * SQRT(1.0 - ((FLOAT(IX)-CX)/RX)**2 
     1                             - ((FLOAT(IY)-CY)/RY)**2)
C            IZ = INT(Z)
C            IF ((IZ.GE.1).AND.(IZ.LT.NZ)) THEN
C               IM3D(IX, IY, IZ)   = IM2D(IX, IY) * (FLOAT(IZ+1)-Z)
C               IM3D(IX, IY, IZ+1) = IM2D(IX, IY) * (Z-FLOAT(IZ))
C            END IF
            DO 5 IZ = 1, NZ
               I = NINT(OSAMP*ABS(FLOAT(IZ)-Z))
               IF (I.LE.LENSINC) THEN
                  IM3D(IX, IY, IZ) = IM2D(IX, IY) * SINC(I)
               ELSE
                  IM3D(IX, IY, IZ) = 0.0
               END IF
  5        CONTINUE
  10     CONTINUE
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
