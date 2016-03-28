C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixlcopy.f	1.1    11/17/92
C
      SUBROUTINE PIXLCOPY (IN, INSTEP, OUT, OUTSTEP, N)
C
CD Copy an array with increments
C
C	IN	LOG	input	Input array
C	INSTEP	INT	input	Step on input
C	OUT	LOG	output	Output array
C	OUTSTEP	INT	input	Step on output
C	N	INT	input	Number of elements
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      LOGICAL	IN(*), OUT(*)
      INTEGER	INSTEP, OUTSTEP, N
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXLCOPY')
C
      INTEGER	I
C=========================================================================
      IF (ERROR) GO TO 999
C
      IF (INSTEP.EQ.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero input step')
         GO TO 999
      END IF
      IF (OUTSTEP.EQ.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero output step')
         GO TO 999
      END IF
C
      DO 10 I = 1, N
         OUT (1+(I-1)*OUTSTEP) = IN (1+(I-1)*INSTEP)
  10  CONTINUE
C
 999  CONTINUE
      END
