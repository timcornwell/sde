
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrisama.f	1.1    3/15/93
C
      SUBROUTINE ARRISAMA (ARRAY, AMAX)
C
CD Finds the ABS max of an array
C
C	ARRAY	CH*(*)	inp	input array
C	AMAX	REAL	inp	maximum of ABS(array)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 3 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ARRAY
      REAL		AMAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRISAMA')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD, I, NT, IM
      CHARACTER*1	T
C
      INTEGER		PIXISAMA
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (ARRAY, NAX, NAXIS, T, ADD)
      NT = 1
      DO 20 I = 1, NAX
         NT = NT * NAXIS(I)
 20   CONTINUE
C
      IF (T .EQ. 'R') THEN
         IM = PIXISAMA (NT, MEMR(ADD), 1)
         AMAX = ABS( MEMR(ADD + IM-1) )
      ELSE
         CALL ERRREPOR(ERRBDARG, ROUTINE, 
     $      'Sorry, no code for ARRAY TYPE '//T)
         GOTO 999
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
