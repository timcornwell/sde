C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdcopy.f	1.3    11/7/90
C
      SUBROUTINE PIXDCOPY (IN, INSTEP, OUT, OUTSTEP, N)
C
CD Copy an array with increments
C
C
C	IN	DBLE	input	Input array
C	INSTEP	INT	input	Step on input
C	OUT	DBLE	output	Output array
C	OUTSTEP	INT	input	Step on output
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      DOUBLE PRECISION	IN(*), OUT(*)
      INTEGER	INSTEP, OUTSTEP, N
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXDCOPY')
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
