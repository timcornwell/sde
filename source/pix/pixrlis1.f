C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrlis1.f	1.2	 7/18/97
C
      SUBROUTINE PIXRLIS1 (ARRAY, N, ISKIP, WHAT)
C
C Lists a real array via MSGPUT
C
C	ARRAY	REAL	input	a real array.  big deal
C	N	INT	input	how long is ARRAY. so what
C	ISKIP	INT	input	print every ISKIP^th element
C	WHAT	CH*(*)	input	description of what array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Aug 1 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL		ARRAY(*)
      CHARACTER*(*)	WHAT
      INTEGER		N, ISKIP
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRLIS1')
C
      INTEGER		I, TOOBIG, TOTAL
      PARAMETER		(TOOBIG = 100)
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL MSGPUT ('Listing ARRAY: '//WHAT, 'L')
C
      IF (ISKIP .LE. 0) ISKIP = 1
      TOTAL = (N-1)/ISKIP + 1
      IF (TOTAL .GT. TOOBIG) THEN
         WRITE(MESSAGE, 500)TOTAL, TOOBIG
 500     FORMAT ('Requested: ',I6,' Actually Listed: ',I3)
         CALL MSGPUT (MESSAGE, 'W')
         TOTAL = TOOBIG
      ENDIF
      TOTAL = (TOTAL - 1) * ISKIP + 1
      DO 100 I = 1, TOTAL, ISKIP
         WRITE (MESSAGE, 600) I, ARRAY(I)
 600     FORMAT (I6, 5X, F13.6)
         CALL MSGPUT (MESSAGE, 'L')
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
