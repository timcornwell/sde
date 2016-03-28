C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixsopos.f	1.1    8/16/91
C
      SUBROUTINE PIXSOPOS (ARR, NX, NY, FMIN, NOISE, DMAX, X, Y, 
     $   DX, DY, N, NFOUND)
C
CD Determines the major regions of emission in ARR
C
C	ARR	REAL(*)	in	Input array
C	NX	INT	in	Size of array
C	NY	INT	in	Size of array
C	FMIN	REAL	in	Minimum flux peak to look for
C	NOISE	REAL	in	Minumum flux for DX,DY extent
C	DMAX	INT	in	Maximum value for DX, DY
C	X	INT(*)	out	X positions
C	Y	INT(*)	out	Y positions
C	DX	INT(*)	out	half width about X
C	DY	INT(*)	out	half width about Y
C	N	INT	in	dimension of X,Y,DX,DY
C	NFOUND	INT	out	number of sources found
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	11 July 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX, NY, N, NFOUND
      REAL		ARR(NX, *), FMIN, NOISE
      INTEGER		X(*), Y(*), DX(*), DY(*), DMAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXSOPOS')
C
      INTEGER		I, IX, IY, DX1, DX2, DY1, DY2, IPEAK
C
      INTEGER		PIXISAMA
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 1000 I = 1, N
C
C Get peak emission
C
         IPEAK = PIXISAMA(NX*NY, ARR, 1) - 1
         X(I) = 1 + MOD(IPEAK, NX)
         Y(I) = 1 + (IPEAK - X(I) + 1)/NX
         IF (ARR (X(I), Y(I)) .LT. FMIN) THEN
            NFOUND = I - 1
            GOTO 980
         ENDIF
C
C Get extent of this source emission      
C
         DX1 = X(I) + DMAX
         DO  100 IX = X(I), MIN (X(I)+DMAX, NX)
            IF (ARR(IX, Y(I)) .LE. NOISE) THEN
               DX1 = IX-1
               GOTO 110
            ENDIF
 100     CONTINUE
 110     CONTINUE
C
         DX2 = X(I) - DMAX
         DO 150 IX = X(I), MAX(X(I)-DMAX, 1), -1
            IF (ARR(IX, Y(I)) .LE. NOISE) THEN
               DX2 = IX+1
               GOTO 160
            ENDIF
 150     CONTINUE
 160     CONTINUE
C
         DX(I) = MIN (DMAX, MAX (ABS(DX1 - X(I)), ABS(DX2 - X(I))))
C
         DY1 = Y(I) + DMAX
         DO  200 IY = Y(I), MIN (Y(I)+DMAX, NY)
            IF (ARR(X(I), IY) .LE. NOISE) THEN
               DY1 = IY-1
               GOTO 210
            ENDIF
 200     CONTINUE
 210     CONTINUE
C
         DY2 = Y(I) - DMAX
         DO 250 IY = Y(I), MAX(Y(I)-DMAX, 1), -1
            IF (ARR(X(I), IY) .LE. NOISE) THEN
               DY2 = IY+1
               GOTO 260
            ENDIF
 250     CONTINUE
 260     CONTINUE
C
         DY(I) = MIN (DMAX, MAX (ABS(DY1 - Y(I)), ABS(DY2 - Y(I))))
C
C Remove this source so we can look for another one
C
         DO 300 IY = Y(I)-DY(I), Y(I)+DY(I)
            DO 280 IX = X(I)-DX(I), X(I)+DX(I)
               ARR(IX,IY) =  0.0
 280        CONTINUE
 300     CONTINUE
C
 1000 CONTINUE
      NFOUND = N
C
C Can jump to here if an error found
C
 980  CONTINUE
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
