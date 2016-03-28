C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxamp2.f	1.1    2/10/93
C
      SUBROUTINE PIXXAMP2 (N, XARR, RARR)
C
CD Extracts AMPLITUDE Squared part of a complex image
C
C	N	INT	input	total number of pixels
C	XARR	X	input	Complex Array
C	RARR	R	input	Real Array
C
C Audit trail:
C             Cloned from pixxreal
C                               R. G. Marson 1 Dec 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		N
      COMPLEX		XARR (N)
      REAL		RARR (N)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXXAMP2')
      INTEGER		I
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
         RARR(I) = REAL( XARR(I) * CONJG(XARR(I)))
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
