C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixidscat.f	1.3    11/7/90
C
      SUBROUTINE PIXIDSCAT (A, XSIZE, YSIZE, X, Y, DATA, NUMPTS)
CD     
C    Recover x, y, data values from a 2D array
C    
C    x	         REAL    output	x values extracted
C    y	         REAL    output	y values extracted
C    data        INTEGER output the data values extracted
C    a           INTEGER input	Input array (2 dimensional)
C    numpts	 INTEGER output/input Max size of x, y, data arrays
C                                    on input, actual size on output
C    xsize       INTEGER input	size of the a array
C    ysize       INTEGER input	size of the a array
C
C    Audit trail:
C    Cloned from pixrdscat               
C                                    R.G. Marson     Mar 12 1990
C    
C---------------------------------------------------------------------
#include	"stdinc.h"
C     
      INTEGER           NUMPTS, XSIZE, YSIZE
      INTEGER           DATA(NUMPTS), A(XSIZE, YSIZE)
      REAL		X(NUMPTS), Y(NUMPTS)
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXIDSCAT')
C     
      INTEGER		I, J, INDX
C=====================================================================
C     
C     If an error on input then exit immediately
C     
      IF (ERROR) GO TO 999
C     
      INDX = 0
      DO 100, I=1, YSIZE
         DO 200, J=1, XSIZE
            IF (A(J,I) .NE. 0) THEN
               INDX = INDX + 1
               IF (INDX.GT.NUMPTS) THEN
                  CALL ERRREPOR(ERRNOMEM, ROUTINE, 
     $                 'Not enough space in x,y, data arrays')
                  GOTO 999
               END IF
               X(INDX) = J
               Y(INDX) = I
               DATA(INDX) = A(J,I)
            END IF
 200     CONTINUE
 100  CONTINUE
C     
C     Can jump to here if an error found
C     
      NUMPTS = INDX
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C     
 999  CONTINUE
      END
