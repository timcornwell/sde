C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)piximag.f	1.1	 3/26/91
C
      SUBROUTINE PIXIMAG (N, CARR, IARR)
C
CD Extracts IMAG part of a complex image
C
C	N	INT	input	total number of pixels
C	CARR	X	input	Complex Array
C	IARR	R	input	Imaginary part of complex Array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Feb 27 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		N
      COMPLEX		CARR (*)
      REAL		IARR (*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXIMAG')
      INTEGER		I
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
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
