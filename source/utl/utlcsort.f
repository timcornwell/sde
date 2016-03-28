C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlcsort.f	1.1	 3/26/93
C
      SUBROUTINE UTLCSORT (N, ARRIN, ARROUT)
C
C A simple minded sort of sort for strings of numbers
C
C	N	INT	inp	Number of elements in ARROUT
C	ARRIN	CH*(*)	inp	1-D array, unsorted
C	ARROUT	CH*(*)	in/out	1-D Array, sorted
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	24 Dec 1992
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		N
      CHARACTER*(*)		ARROUT(*), ARRIN(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLCSORT')
C
      INTEGER		I, J
      CHARACTER*(SYSMXNAM)	A
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 20 I = 1, N
         ARROUT(I) = ARRIN(I)
 20   CONTINUE
      DO 100 J=2,N
         A=ARROUT(J)
         DO 50 I=J-1,1,-1
            IF( ARROUT(I) .LE. A ) GOTO 70
            ARROUT(I+1)=ARROUT(I)
 50      CONTINUE
         I=0
 70      ARROUT(I+1)=A
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
 

