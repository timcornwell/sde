C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixircpy.f	1.1    9/25/93
C
      SUBROUTINE PIXIRCPY (IN, INSTEP, OUT, OUTSTEP, N)
C
CD Copy an array with increments
C
C	IN	DBLE	input	Input array
C	INSTEP	INT	input	Step on input
C	OUT	REAL	output	Output array
C	OUTSTEP	INT	input	Step on output
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Cloned from PIXDRCPY
C				D.S.Briggs	Sept 7 1993
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER	IN(*)
      REAL	OUT(*)
      INTEGER	INSTEP, OUTSTEP, N
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXIRCPY')
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
