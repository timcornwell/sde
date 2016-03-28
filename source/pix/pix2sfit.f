C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2sfit.f	1.2	 11/27/91
C
      SUBROUTINE PIX2SFIT (IN, MASK, AMIN, NX, NY, RX, RY, A, B)
C
CD Fits A*X + B*Y to a 2-D real array
C  Units of slope = Units of IN / pixel
C  If some region is zeroed out, do not include in the fit
C
C	IN	REAL	input	Real, 2-D array to fit to
C	MASK	REAL	input	Mask array
C	AMIN	REAL	in	clip value for mask
C	NX	INT	input	size of array in X
C	NY	INT	input	size of array in Y
C	RX	REAL	input	center of array, X
C	RY	REAL	input	center of array, Y
C	A	REAL	out	fitted Slope in X
C	B	REAL	out	fitted Slope in Y
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 5 1991
C	Added MASK
C				M.A.H.		Nov 27 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	NX, NY
      REAL	RX, RY, A, B, IN(NX, *), MASK(NX, *), AMIN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2SFIT')
C
      REAL	SX, SY, SZ, SX2, SY2, SXZ, SYZ, X, Y, Z
      INTEGER	IX, IY, N
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      SX = 0.0
      SY = 0.0
      SZ = 0.0
      SX2 = 0.0
      SY2 = 0.0
      SXZ = 0.0
      SYZ = 0.0
      N = 0
      DO 100 IY = 1, NY
         DO 50 IX = 1, NX
            IF (MASK(IX, IY) .GT. AMIN) THEN
               N = N + 1
               Z = IN(IX, IY)
               X = (IX - RX)
               Y = (IY - RY)
               SZ = SZ + Z
               SX = SX + X
               SY = SY + Y
               SX2 = SX2 + X*X
               SY2 = SY2 + Y*Y
               SXZ = SXZ + X * Z
               SYZ = SYZ + Y * Z
            ENDIF
 50      CONTINUE
 100  CONTINUE
C
      IF (N .EQ. 0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No pixels for fit')
         GOTO 990
      ENDIF
      A = ( N * SXZ - SX*SZ ) / ( N * SX2 - SX*SX )
      B = ( N * SYZ - SY*SZ ) / ( N * SY2 - SY*SY )
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
