C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrmode.f	1.1    10/29/91
C
      SUBROUTINE ARRMODE (NAME, EPS)
C
CD Find the mode of the array.
C
C Due to practical space limitations, this algorithm is not guaranteed to
C succeed.  It's primary use is intended to be digging out reasonable
C guesses to estimated parameters, where there is one dominant value with
C a small number of alternatives.
C
C	NAME	CH*(*)	input	Name of directory entry
C	EPS	REAL	input	Fractional accuracy for two numbers to be equal
C	NAME/MODE	R/D/I/X	Mode of array.  Type is same as input array.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Sept 27 1991
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
      REAL		EPS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRMODE')
C
      INTEGER		BUFSIZ
      PARAMETER		(BUFSIZ=500)
C
      REAL		RMODE
      INTEGER		NAX, NAXIS(SYSMXDIM), INADD, BADD, AADD, IADD,
     $   		NT, I
      CHARACTER		ATYPE*1
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, INADD)
      NT = 1
      DO 100 I = 1, NAX
         IF (NAXIS(I) .GT. 0)  NT = NT * NAXIS(I)
 100  CONTINUE
C
      NAX = 1
      NAXIS(1) = MIN (BUFSIZ, NT)
      CALL DATMAKAR ('Mode Buff', NAX, NAXIS, ATYPE, BADD)
      CALL DATMAKAR ('Mode Acc', NAX, NAXIS, ATYPE, AADD)
      CALL DATMAKAR ('Mode Cntr', NAX, NAXIS, 'I', IADD)
      IF (ERROR) GO TO 990
C
      IF (ATYPE.EQ.'R') THEN
         CALL PIXRMODE (MEMR(INADD), NT, MEMR(BADD), MEMR(AADD),
     $      MEMI(IADD), NAXIS(1), EPS, RMODE)
         CALL DATPUTR (NAME, 'MODE', RMODE, 1)
      ELSE
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Only type R implemented')
         GO TO 999
      END IF
C
      CALL DATDELAR ('Mode Buff')
      CALL DATDELAR ('Mode Acc')
      CALL DATDELAR ('Mode Cntr')
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
