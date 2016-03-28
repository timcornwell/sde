C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixqu2x.f	1.1	 2/14/91
C
      SUBROUTINE PIXQU2X (N, CARR, RARR, IARR)
C
CD Converts the two real arrays RARR and IARR into the real and imaginary
C parts of the complex array CARR
C
C CALL PIXQU2X (N, CARR, RARR, IARR)
C
C	N	INT	input	total number of pixels
C	CARR	X	output	Complex Array 
C	RARR	R	input	Rael part of complex Array
C	IARR	R	input	Imaginary part of complex Array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Dec 3 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		N
      COMPLEX		CARR (*)
      REAL		RARR (*), IARR(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXQU2X')
      INTEGER		I
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
         CARR(I) = CMPLX (RARR(I), IARR(I))
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
