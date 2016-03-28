C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixvlbap.f	1.2    11/7/90
C
      SUBROUTINE PIXVLBAP (NVLBA, VLBAARR, RADMAX)
C
CD Make the VLBA power pattern in a work array
C
C
C	NVLBA	INT	inp	Number of elements
C	VLBAARR	REAL	output	Array
C	RADMAX	REAL	input	Maximum value of argument
C Audit trail:
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NVLBA
      REAL		RADMAX, VLBAARR(*)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXVLBAP')
C
      REAL		BESJ1, P, X, RSCL
      INTEGER		IVLBA
C=======================================================================
      P(X) = (((100.*2.*BESJ1(X)/X)-(2.*BESJ1(X*10.)/(X*10.)))/99.)**2
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      RSCL = FLOAT(NVLBA-1) / RADMAX
C
      VLBAARR(1) = 1.0
      DO 5 IVLBA = 2, NVLBA
         VLBAARR(IVLBA) = P(FLOAT(IVLBA-1)/RSCL)
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
