C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C  @(#)pixredit.f	1.1 5/21/91
C
      SUBROUTINE PIXREDIT (AIN, AMIN, AMAX, AOUT, N)
C
CD EDIT an array
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
         IF((AIN(I).LE.AMAX).AND.(AIN(I).GE.AMIN)) THEN
            AOUT(I) = AIN(I)
         ELSE
            AOUT(I) = 0.0
         ENDIF
 10   CONTINUE
C
      END
