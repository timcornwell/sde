C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixsinc.f	1.1	 5/28/91
C
      SUBROUTINE PIXSINC (NSINC, SINC, RADMAX)
C
CD Pixel level routine for making a SINC function
C
C	NSINC	INT	inp	Number of elements
C	SINC	REAL	output	Array
C	RADMAX	INT	input	Maximum value of argument
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 28 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NSINC
      REAL		RADMAX, SINC(*)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXSINC')
C
      REAL		P, X, RSCL
      INTEGER		I
C=======================================================================
      P(X) = SIN(3.1415927 * X)/(3.1415927 * X)
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      RSCL = FLOAT(NSINC-1) / RADMAX
C
      SINC(1) = 1.0
      DO 5 I = 2, NSINC
         SINC(I) = P(FLOAT(I-1)/RSCL)
  5   CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
