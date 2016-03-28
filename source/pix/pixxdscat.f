C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxdscat.f	1.3    1/30/91
C
      SUBROUTINE PIXXDSCAT (A, XSIZE, YSIZE, X, Y, DATA, NUMPTS)
C
CD    Recover x, y, data values from a 2D array
C    
C    x	         REAL	 output	x values extracted
C    y	         REAL	 output	y values extracted
C    data        COMPLEX output the data values extracted
C    a           COMPLEX input	Input array (2 dimensional)
C    numpts	 INTEGER output/input Max size of x, y, data arrays
C                                    on input, actual size on output
C    xsize       INTEGER input	size of the a array
C    ysize       INTEGER input	size of the a array
C
C    Audit trail:
C	Cloned from pixidscat               
C       				R.G. Marson     Mar 12 1990
C	Removed verboten complex equality test
C					T.J. Cornwell	Jan 30 1991
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C     
      INTEGER           NUMPTS, XSIZE, YSIZE
      COMPLEX           DATA(NUMPTS), A(XSIZE, YSIZE)
      REAL		X(NUMPTS), Y(NUMPTS)
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXXDSCAT')
C     
      INTEGER		I, J, INDX
C=====================================================================
C     
C     If an error on input then exit immediately
C     
      IF (ERROR) GO TO 999
C     
      INDX = 0
      DO 100 I=1, YSIZE
         DO 200 J=1, XSIZE
            IF (ABS(A(J,I)) .NE. 0.0 ) THEN
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
