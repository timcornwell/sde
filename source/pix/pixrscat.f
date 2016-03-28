C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrscat.f	1.2    11/7/90
C
      SUBROUTINE PIXRSCAT (X, Y, DATA, NUMPTS, OUT, XSIZE, YSIZE)
CD     
CD    Plot x vs y onto array out (Real arrays only)
C     
CS    Arguments: CALL pixrscat (x, y, numpts, out, xsize, ysize)
CS    
CS    x	         REAL	input	x values
CS    y	         REAL	input	y values
CS    data       REAL   input   the data values to plot
CS    out	 REAL	input	Output array (2 dimensional)
CS    numpts	 INTEGER input	number of points to plot
CS    xsize      INTEGER input	size of the output array
CS    ysize      INTEGER input	size of the output array
CS
C    Audit trail:
C    Cloned from one of the pix... routines                
C                                    R.G. Marson     Jan 13 1990
C    Fixed up some of the comments
C                                    R.G Marson      Feb 9 1990
C    Added the data arguement
C                                    R.G Marson      Feb 16 1990
C    
C---------------------------------------------------------------------
#include	"stdinc.h"
C     
      INTEGER           NUMPTS, XSIZE, YSIZE
      REAL		OUT(XSIZE,YSIZE), X(NUMPTS), Y(NUMPTS)
      REAL              DATA(NUMPTS)
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRSCAT')
C     
      INTEGER		XI, YI, I
C=====================================================================
C     
C     If an error on input then exit immediately
C     
      IF (ERROR) GO TO 999
C     
      DO 100, I=1, NUMPTS 
         XI = NINT (MIN (MAX(X(I),1.0), FLOAT(XSIZE)))
         YI = NINT (MIN (MAX(Y(I),1.0), FLOAT(YSIZE)))
         OUT(XI, YI) = OUT(XI, YI) + DATA(I)
 100  CONTINUE
C     
C     Can jump to here if an error found
C     
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C     
 999  CONTINUE
      END
