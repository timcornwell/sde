C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixblint.f	1.2    24 Feb 1995
C
      SUBROUTINE PIXBLINT (ARR, NX, NY, REFX, REFY, CELLX, CELLY, 
     $   X, Y, AINT, ANEAR)
C
CD Bi-Linear Interpolation for the value of an array
C
C	ARR	R(*)	inp	Real Input Array
C	NX	INT	inp	X dimension of arr
C	NY	INT	inp	Y dimension of arr
C	REFX	REAL	inp	X Ref Pixel
C	REFY	REAL	inp	Y Ref Pixel
C	CELLX	REAL	inp	X cell size
C	CELLY	REAL	inp	Y cell size
C	X	REAL	inp	X value wrt REFX
C	Y	REAL	inp	Y value wrt REFY
C	AINT	REAL	out	Interpolated value
C	ANEAR	REAL	out	Closest value
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	Feb 26 1993
C       Changed FLOAT(IX-REFX) to (FLOAT(IX)-REFX) to appease the DEC
C       Alpha F77 compiler.
C                               J.D. Ellithorpe Oct 20 1994
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX, NY
      REAL		ARR(NX, *)
      REAL		REFX, REFY, CELLX, CELLY
      REAL		X, Y, AINT, ANEAR
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXBLINT')
C
      INTEGER		IX, IY, IXF, IYF
      REAL		T, U
C
      INTEGER		UTLGINT
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IXF = CELLX/ABS(CELLX)
      IYF = CELLY/ABS(CELLY)
C
      IX = UTLGINT(X/CELLX + REFX)
      IY = UTLGINT(Y/CELLY + REFY)
C
      IF (IX .LT. 2  .OR. IX .GT. (NX-2)) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'X out of bounds')
         GOTO 990
      ENDIF
      IF (IY .LT. 2  .OR. IY .GT. (NY-2)) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Y out of bounds')
         GOTO 990
      ENDIF
C
      T = (X - CELLX * ( FLOAT(IX) - REFX ) ) / ABS(CELLX)
      U = (Y - CELLY * ( FLOAT(IY) - REFY ) ) / ABS(CELLY)
C
      AINT = (1-T)*(1-U) * ARR (IX, IY)
     $   +    T  *(1-U) * ARR (IX+IXF, IY)
     $   +    T  *  U   * ARR (IX+IXF, IY+IYF)
     $   +  (1-T)*  U   * ARR (IX, IY+IYF)
C
      ANEAR = ARR ( NINT(X/CELLX + REFX), NINT(Y/CELLY + REFY) )
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
