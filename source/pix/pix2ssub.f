C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2ssub.f	1.2	 11/27/91
C
      SUBROUTINE PIX2SSUB (ARR, MASK, AMIN, NX, NY, RX, RY, A, B)
C
CD Subtracts a the plane ARR = A*X + B*Y from ARR
C  A, B are in units of:   UNITS of ARR / pixel
C
C	IN	REAL	in/out	Real, 2-D array to fit to
C	MASK	REAL	input	Mask array
C	AMIN	REAL	in	Min for mask
C	NX	INT	input	size of array in X
C	NY	INT	input	size of array in Y
C	RX	REAL	input	center of array, X
C	RY	REAL	input	center of array, Y
C	A	REAL	in	fitted slope, X
C	B	REAL	in	fitted slope, Y
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 5 1991
C	Added mask
C				M A H	 	Nov 27 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	NX, NY
      REAL	RX, RY, A, B, ARR(NX, *), MASK(NX, *), AMIN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2SSUB')
C
      INTEGER	IX, IY
      REAL	BY
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 100 IY = 1, NY
         BY = B * (IY - RY)
         DO 50 IX = 1, NX
            IF (MASK(IX, IY) .GT. AMIN) THEN
               ARR(IX, IY) = ARR(IX, IY) - BY - A * (IX - RX)
            ENDIF
 50      CONTINUE
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
