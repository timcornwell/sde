C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixisel.f	1.3    11/7/90
C
      SUBROUTINE PIXISEL (A, W, N, NA, NVALID)
C
CD Select valid points
C
C
C	A(*)	INTEGER	input	Array of points
C	W(*)	REAL	input	Weights
C	N	INT	input	Number of points
C	A(*)	INTEGER	output	Validated points
C	NVALID	INT	output	Number of points > 0
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	N, NVALID
      INTEGER	A(*), NA(*)
      REAL	W(*)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXISEL')
C
      INTEGER	I
C=====================================================================
      NVALID = 0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
         IF (W(I).GT.0.0)  THEN
           NVALID = NVALID + 1
           NA(NVALID) = A(I)
         END IF
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
