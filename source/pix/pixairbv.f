C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixairbv.f	1.3    11/7/90
C
      SUBROUTINE PIXAIRBV (NAIRY, AIRYARR, RADMAX)
C
CD Make the 1/10 blocked VOLTAGE pattern in a work array
C
C
C	NAIRY	INT	inp	Number of elements
C	AIRYARR	REAL	output	Array
C	RADMAX	REAL	input	Maximum value of argument
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NAIRY
      REAL		RADMAX, AIRYARR(*)
C
      CHARACTER*(*)	ROUTINE
C
      PARAMETER		(ROUTINE = 'PIXAIRBV')
C
      REAL		BESJ1, P, X, RSCL
      INTEGER		IAIRY
C=======================================================================
      P(X) = ((100.*2.*BESJ1(X)/X)-(2.*BESJ1(X*10.)/(X*10.)))/99.
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      RSCL = FLOAT(NAIRY-1) / RADMAX
C
      AIRYARR(1) = 1.0
      DO 5 IAIRY = 2, NAIRY
         AIRYARR(IAIRY) = P(FLOAT(IAIRY-1)/RSCL)
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
