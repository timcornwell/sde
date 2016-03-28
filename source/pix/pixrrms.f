C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrrms.f	1.3    11/7/90
C
      SUBROUTINE PIXRRMS (A1, N, RMS)
C
CD Find rms value
C
C
C	A1	REAL	input	Real array
C	N	INT	input	Number of elements
C	RMS	REAL	output	RMS
C	A2	REAL	output	Real array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRRMS')
C
      REAL		A1(*), RMS
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      RMS = 0.0
C
      IF (ERROR) GO TO 999
C
      IF (N.LE.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'N = 0')
         GO TO 999
      END IF
C
      DO 10 I = 1, N
         RMS = RMS + A1(I)**2
 10   CONTINUE
C
      RMS = SQRT(RMS/N)
C
 999  CONTINUE
      END
