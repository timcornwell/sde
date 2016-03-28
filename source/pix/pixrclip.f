C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrclip.f	1.3    11/7/90
C
      SUBROUTINE PIXRCLIP (AIN, AMIN, AMAX, AOUT, N)
C
CD Clip an array
C
C
C	AIN	REAL	input	Real array
C	AMIN	REAL	input	Min allowed
C	AMAX	REAL	input	Max allowed
C	AOUT	REAL	input	Real array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		AIN(*), AOUT(*), AMIN, AMAX
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         AOUT(I) = MAX(AMIN, MIN (AMAX, AIN(I)))
 10   CONTINUE
C
      CONTINUE
      END
