C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrvali.f	1.2    11/7/90
C
      SUBROUTINE PIXRVALI (A, N, NVALID)
C
CD Find number of valid points 
C
C
C	A(*)	REAL	input	Array of points
C	N	INT	input	Number of points
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
      REAL	A(*)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'PIXRVALI')
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
         IF (A(I).GT.0.0) NVALID = NVALID + 1
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
