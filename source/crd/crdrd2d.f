C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdrd2d.f	1.1    6/5/93
C
      SUBROUTINE CRDRD2D (RA, DEC, RAD, DECD)
C
CD Convert from RA,Dec real(3) sky coordinates to degrees.
C
C	RA	REAL(3)	input	HMS
C	DEC	REAL(3)	input	DMS
C	RAD	DBLE	output	RA in degrees
C	DECD	DBLE	output	DEC in degrees
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	May 10 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDRD2D')
C
      REAL		RA(3), DEC(3)
      DOUBLE PRECISION	RAD, DECD
C
      REAL		S
C=====================================================================
      IF (ERROR) GO TO 999
C
      S = SIGN(1.0, RA(1))
      RA(1) = ABS(RA(1))
      RAD = (DBLE(RA(1)) + DBLE(RA(2)) / 60.D0 +
     $   DBLE(RA(3)) / 3600.D0) * 15.D0
      IF (S.LT.0.0) RAD = -RAD
C
      S = SIGN(1.0, DEC(1))
      DEC(1) = ABS(DEC(1))
      DECD = DBLE(DEC(1)) + DBLE(DEC(2)) / 60.D0 +
     $   DBLE(DEC(3)) / 3600.D0
      IF (S.LT.0.0) DECD = -DECD
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
