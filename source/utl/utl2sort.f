C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utl2sort.f	1.2	 7/20/92
C
      SUBROUTINE UTL2SORT (N, AIN, BIN, AOUT, BOUT)
C
C Sorts order pairs AIN, BIN according to AIN
C
C	N	INT	inp	Number of elements in ARROUT
C	AIN	REAL	inp	1-D array, unsorted
C	BIN	REAL	inp	1-D array, unsorted
C	AOUT	REAL	in/out	1-D Array, sorted
C	BOUT	REAL	in/out	1-D Array, sorted
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Aug 13 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		N
      REAL		AIN(*), AOUT(*), BIN(*), BOUT(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTL2SORT')
C
      INTEGER		I, J
      REAL		A, B
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 20 I = 1, N
         AOUT(I) = AIN(I)
         BOUT(I) = BIN(I)
 20   CONTINUE
      DO 100 J=2,N
         A=AOUT(J)
         B=BOUT(J)
         DO 50 I=J-1,1,-1
            IF(ABS(AOUT(I)).LE.ABS(A)) GOTO 70
            AOUT(I+1)=AOUT(I)
            BOUT(I+1)=BOUT(I)
 50      CONTINUE
         I=0
 70      AOUT(I+1)=A
         BOUT(I+1)=B
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
 

