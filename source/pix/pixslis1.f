C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixslis1.f	1.2    7/18/97
C
      SUBROUTINE PIXSLIS1 (ARRAY, N, ISKIP, WHAT)
C
C Lists a character array via MSGPUT
C
C	ARRAY	C*(SYSMXDIM)(*)	input	a character array.  big deal
C	N	INT	input	how long is ARRAY. so what
C	ISKIP	INT	input	print every ISKIP^th element
C	WHAT	CH*(*)	input	description of what array
C
C Audit trail:
C	Original version: 
C	Cloned from PIXRLIS1
C				D.S.Briggs	Sept 16 1993
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(SYSMXNAM)	ARRAY(*)
      CHARACTER*(*)	WHAT
      INTEGER		N, ISKIP
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXSLIS1')
C
      INTEGER		I, TOOBIG, TOTAL, STRLEN
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
         WRITE (MESSAGE, 600) I, ARRAY(I)(1:STRLEN(ARRAY(I)))
 600     FORMAT (I6, 5X, '''',A,'''')
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
