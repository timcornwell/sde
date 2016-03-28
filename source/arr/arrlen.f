C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrlen.f	1.1    6/7/93
C
      SUBROUTINE ARRLEN (A1, LEN)
C
CD Sum the elements in the array in quadrature  (Euclidean length for vector)
C
C	A1	CH*(*)	input	Name of array
C	SUM	REAL	output	SUM
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	13 Mar 1993
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1
      REAL		LEN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRLEN')
C=====================================================================
      IF (ERROR) GO TO 999
C     
      CALL ARRMULT (A1, A1, 'ARRLEN-TEMP')
      CALL ARRSUM ('ARRLEN-TEMP', LEN)
      LEN = SQRT(LEN)
      CALL DATDELET ('ARRLEN-TEMP')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
