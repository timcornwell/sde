C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixiscat.f	1.3    11/7/90
C
      SUBROUTINE PIXISCAT (X, Y, DATA, NUMPTS, OUT, XSIZE, YSIZE)
CD     
CD    Plot x vs y onto array out (Integer arrays only)
C     
CS    Arguments: CALL pixiscat (x, y, numpts, out, xsize, ysize)
CS    
CS    x	         INTEGER input	x values
CS    y	         INTEGER input	y values
CS    data	 REAL    input	Data array
CS    out	 REAL    input	Output array (2 dimensional)
CS    numpts	 INTEGER input	number of points to plot
CS    xsize      INTEGER input	size of the output array
CS    ysize      INTEGER input	size of the output array
CS
C    Audit trail:
C    Cloned from pixrscat
C                                    R.G. Marson     Feb 9 1990
C    Modified to include the data arguement
C                                    R.G. Marson     feb 16 1990
C    
C    
C---------------------------------------------------------------------
#include	"stdinc.h"
C     
      INTEGER NUMPTS, XSIZE, YSIZE
      INTEGER X(NUMPTS), Y(NUMPTS)
      REAL OUT(XSIZE,YSIZE), DATA(NUMPTS)
C     
      CHARACTER*(*) ROUTINE
      PARAMETER (ROUTINE = 'PIXISCAT')
C     
      INTEGER XI, YI, I
C=====================================================================
C     
C     If an error on input then exit immediately
C     
      IF (ERROR) GO TO 999
C     
      DO 100, I=1, NUMPTS 
         XI = MIN (MAX(X(I),1), XSIZE)
         YI = MIN (MAX(Y(I),1), YSIZE)
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
