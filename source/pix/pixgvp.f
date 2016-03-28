C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixgvp.f	1.3    11/7/90
C
      SUBROUTINE PIXGVP (NGPB, GPBARR, RADMAX)
C
CD Make the Gaussian disk in a work array
C
C
C	NGPB	INT	inp	Number of elements
C	GPBARR	REAL	output	Array
C	RADMAX	REAL	input	Maximum value of argument
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	This routine now does a gaussian voltage pattern.
C				M.A.Holdaway	Feb 13, 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NGPB
      REAL		RADMAX, GPBARR(*)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXGPB')
C
      REAL		BESJ1, P, X, RSCL
      INTEGER		IGPB
C=======================================================================
      P(X) = SQRT ( EXP (-X**2) )
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      RSCL = FLOAT(NGPB-1) / RADMAX
C
      GPBARR(1) = 1.0
      DO 5 IGPB = 2, NGPB
         GPBARR(IGPB) = P(FLOAT(IGPB-1)/RSCL)
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
