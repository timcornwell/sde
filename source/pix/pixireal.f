C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixireal.f	1.1	 8/7/91
C
      SUBROUTINE PIXIREAL (N, IARR, RARR)
C
CD Converts integer array to real 
C
C	N	INT	input	total number of pixels
C	IARR	INT(*)	input	Integer Array
C	RARR	R(*)	output	Rael part of complex Array
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
      INTEGER		IARR (*)
      REAL		RARR (*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXIREAL')
      INTEGER		I
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
         RARR(I) = IARR(I)
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
