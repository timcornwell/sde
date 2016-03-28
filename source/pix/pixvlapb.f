C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixvlapb.f	1.1	 8/12/91
C
      SUBROUTINE PIXVLAPB (NPB, PBARR, RADMAX)
C
CD Make the VLA PB in a work array
C
C
C	NPB	INT	inp	Number of elements
C	PBARR	REAL	output	Array
C	RADMAX	REAL	input	Maximum value of argument
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Aug 12 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NPB
      REAL		RADMAX, PBARR(*)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXVLAPB')
C
      REAL		P, X, RSCL
      INTEGER		IPB
C=======================================================================
      P(X) = 1.0 - 0.1329074E-2*(X/.076154355)**2 
     $           + 0.6920305E-6*(X/.076154355)**4
     $           - 0.1434144E-9*(X/.076154355)**6
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      RSCL = FLOAT(NPB-1) / RADMAX
C
      PBARR(1) = 1.0
      DO 5 IPB = 2, NPB
         PBARR(IPB) = P(FLOAT(IPB-1)/RSCL)
         IF (PBARR(IPB) .LT. 0.0) PBARR(IPB) = 0.0
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
