C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixcs.f	1.3    11/7/90
C
      SUBROUTINE PIXCS (NCS, CSARR)
C
CD Make the cos/sin lookup table in a work array
C
C
C	NCS	INT	inp	Number of elements
C	CSARR	REAL	output	Array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NCS
      REAL		CSARR(*)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXCS')
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      REAL	RADMAX
      PARAMETER	(RADMAX = 2.5 * PI)
C
      REAL		P, X, RSCL
      INTEGER		ICS, ZERO, HALF
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
