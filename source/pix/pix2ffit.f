C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2ffit.f	1.2	 11/27/91
C
      SUBROUTINE PIX2FFIT (IN, MASK, AMIN, NX, NY, RX, RY, C)
C
CD Fits C*(X^2 + Y^2) to a 2-D real array
C  Units of C = Units of IN / pixel^2
C  If some region is zeroed out, do not include in the fit
C
C	IN	REAL	input	Real, 2-D array to fit to
C	MASK	REAL	input	Mask array
C	AMIN	REAL	in	clip value for mask
C	NX	INT	input	size of array in X
C	NY	INT	input	size of array in Y
C	RX	REAL	input	center of array, X
C	RY	REAL	input	center of array, Y
C	C	REAL	out	fitted coeficient for parabola
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 5 1991
C	Aded mask		M.A.H.		Nov 27 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	NX, NY
      REAL	RX, RY, C, IN(NX, *), MASK(NX, *), AMIN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2SFIT')
C
      REAL	R2, Z, SR2, SR4, SZ, SR2Z, Y2
      INTEGER	IX, IY, N
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      SR2 = 0.0
      SR4 = 0.0
      SR2Z = 0.0
      SZ = 0.0
      N = 0
      DO 100 IY = 1, NY
         Y2 = (IY - RY)*(IY - RY) 
         DO 50 IX = 1, NX
            IF (MASK(IX, IY) .GT. AMIN) THEN
               N = N + 1
               Z = IN(IX, IY)
               R2 = (IX - RX)*(IX - RX) + Y2 
               SZ = SZ + Z
               SR2 = SR2 + R2
               SR4 = SR4 + R2*R2
               SR2Z = SR2Z + R2 * Z
            ENDIF
 50      CONTINUE
 100  CONTINUE
C
      IF (N .EQ. 0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No pixels for fit')
         GOTO 990
      ENDIF
      C = ( N * SR2Z - SR2*SZ ) / ( N * SR4 - SR2*SR2 )
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
