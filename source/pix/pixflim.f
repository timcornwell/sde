C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixflim.f	1.3    11/7/90
C
      SUBROUTINE PIXFLIM (A, NB, NL, SLIM)
C
CD Find a limit so that at most NL members have an absolute value
C greater than it. 
C
C
C	A	INT(*)	input	Histogram array
C	NB	INT	input	Number of boxs
C	NL	INT	input	Number of elements
C	SLIM	INT	output	Number
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		A(*)
      INTEGER		NL, NB, SLIM
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXFLIM')
C
      INTEGER		I, INDEX, NTOTAL
C=====================================================================
      IF (ERROR) GO TO 999
C
      NTOTAL = 0
      DO 10 I = NB, 1, -1
         NTOTAL = NTOTAL + A(I)
         IF (NTOTAL.GE.NL) THEN
            SLIM = I + 1
            GO TO 11
         END IF
 10   CONTINUE
      SLIM = 1
 11   CONTINUE
C
      IF (SLIM.GT.NB) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Too many points')
         GO TO 999
      END IF
C
  999 CONTINUE
      END
