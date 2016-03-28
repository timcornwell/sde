C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdesph.f	1.3    11/7/90
C
      SUBROUTINE PIXDESPH (IM3D, NX, NY, NZ, IM2D, RX, RY, RZ, 
     1   CX, CY, CZ, INTFN, WIDTH)
C
CD Project a sphere onto the plane X-Y. The center of the sphere
C is at CX, CY, CZ pixels. Either Gaussian-fitting or Sinc-interpolation 
C can be used.
C
C
C	IM3D	REAL	input	Input 3D sphere
C	NX	INT	input	Axis info
C	IM2D	REAL	output	Output 2D plane
C	RX	REAL	input	Radius of cube in X direction
C	CX,..	REAL	input	Center of sphere in IM3D
C	INTFN	CHAR	input	Name of function: 'GAUSS' or 'SINC'
C	WIDTH	REAL	input	Width of INTFN in pixels FWHM for GAUSS
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added Gaussian fitting
C				T.J. Cornwell	August 23 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	INTFN
      INTEGER		NX, NY, NZ
      REAL		IM3D(NX, NY, *), IM2D(NX, *)
      REAL		RX, RY, RZ, CX, CY, CZ, WIDTH
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXDESPH')
C
      INTEGER		IX, IY, IZ, I
      REAL		Z, X, PI, OSAMP, RFACT, SUMWT
      INTEGER		LENIFN
      PARAMETER		(LENIFN = 1024)
      REAL 		IFN(0:LENIFN)
      DATA		PI	/3.14159274101257/
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF ((INTFN.EQ.'SINC').OR.(WIDTH.LE.0.0)) THEN
C
C Sinc function interpolation
C
         OSAMP = FLOAT(LENIFN)/FLOAT(NZ)
         IFN(0) = 1.0
         DO 1 IZ = 1, LENIFN
            X = PI * FLOAT(IZ) / OSAMP
            IFN(IZ) = SIN(X)/X
 1       CONTINUE
         DO 20 IY = 1, NY
            DO 10 IX = 1, NX
               Z = CZ + RZ * SQRT(1.0 - ((FLOAT(IX)-CX)/RX)**2 
     1                                - ((FLOAT(IY)-CY)/RY)**2)
               SUMWT = 0.0
               IM2D(IX, IY) = 0.0
               DO 5 IZ = 1, NZ
                  I = NINT(OSAMP*ABS(FLOAT(IZ)-Z))
                  IF (I.LE.LENIFN) THEN
                     IM2D(IX, IY) = IM2D(IX, IY) + IM3D(IX, IY, IZ) 
     1                  * IFN(I)
                  END IF
  5           CONTINUE
  10       CONTINUE
  20     CONTINUE
      ELSE
C
C Gaussian function fitting: goes out to 5.0 FWHM
C
         OSAMP = FLOAT(LENIFN) / 5.0
         RFACT = 4.0 * LOG(2.0) / WIDTH**2
         IFN(0) = 1.0
         DO 2 IZ = 1, LENIFN
            X = FLOAT(IZ) / OSAMP
            IFN(IZ) = EXP(-RFACT * X**2)
 2       CONTINUE
         DO 120 IY = 1, NY
            DO 110 IX = 1, NX
               Z = CZ + RZ * SQRT(1.0 - ((FLOAT(IX)-CX)/RX)**2 
     1                                - ((FLOAT(IY)-CY)/RY)**2)
               SUMWT = 0.0
               IM2D(IX, IY) = 0.0
               DO 105 IZ = 1, NZ
                  I = NINT(OSAMP*ABS(FLOAT(IZ)-Z))
                  IF (I.LE.LENIFN) THEN
                     IM2D(IX, IY) = IM2D(IX, IY) + IM3D(IX, IY, IZ) 
     1                  * IFN(I)
                     SUMWT = SUMWT + IFN(I)**2
                  END IF
  105         CONTINUE
C
C Somewhat arbitrary cutoff
C
              IF (SUMWT.GT.0.5) THEN
                 IM2D(IX, IY) = IM2D(IX, IY) / SUMWT
              END IF
  110       CONTINUE
  120    CONTINUE
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
