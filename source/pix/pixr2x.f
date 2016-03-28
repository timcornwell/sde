C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixr2x.f	1.1    1/31/93
C
      SUBROUTINE PIXR2X (RARR, CARR, N)
C
CD Converts a real array to a complex one
C
C	N	INT	input	total number of pixels
C	CARR	X	input	Complex Array
C	RARR	R	input	Real part of complex Array
C
C Audit trail:
C       Cloned from pixxreal
C				R.G. Marson	Apr 17 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		N
      COMPLEX		CARR (*)
      REAL		RARR (*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXR2X')
      INTEGER		I
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
         CARR(I) = CMPLX(RARR(I), 0. )
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
