C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrcl2z.f	1.1    11/7/94
C
      SUBROUTINE PIXRCL2Z (AIN, AMIN, AMAX, AOUT, N)
C
CD Clip an array symmetrically around zero.
C  allowed values are (-AMAX, -AMIN) U (AMIN, AMAX)
C
C	AIN	REAL	input	Real array
C	AMIN	REAL	input	Min allowed
C	AMAX	REAL	input	Max allowed
C	AOUT	REAL	input	Real array
C	N	INT	input	Number of elements
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Aug 4 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		AIN(*), AOUT(*), AMIN, AMAX
      INTEGER		N
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         IF (AIN(I).GE.0.0) THEN
            AOUT(I) = MAX(AMIN, MIN(AMAX, AIN(I)))
         ELSE
            AOUT(I) = MAX(-AMAX, MIN(-AMIN, AIN(I)))
         END IF
 10   CONTINUE
C
      CONTINUE
      END
