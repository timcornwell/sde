C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixx2qu.f	1.1	 2/14/91
      SUBROUTINE PIXX2QU (N, CARR, RARR, IARR)
C
CD Converts a 2D complex array into real arrays of the real and imaginary parts
C
C CALL PIXX2QU (N, CARR, RARR, IARR)
C
C	N	INT	input	total number of pixels
C	CARR	X	input	Complex Array
C	RARR	R	input	Rael part of complex Array
C	IARR	R	input	Imaginary part of complex Array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Feb 5 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		N
      COMPLEX		CARR (N)
      REAL		RARR (N), IARR(N)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXX2QU')
      INTEGER		I
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
         RARR(I) = REAL ( CARR(I) )
         IARR(I) = AIMAG ( CARR(I) )
 10   CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
