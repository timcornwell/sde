C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visgtip.f	1.1    12/27/91
C
      SUBROUTINE VISGTIP (TIME, BASELINE, NPTS, DT, NDT, WORK, MAXANT)
C
CD Get integration time from visibility database.  (pixel level)
C
C       TIME	REAL(NPTS)	input	Input times
C	BASELINE REAL(NPTS)	input	Input baselines
C	NPTS	INT		input	Size of vis array
C	DT	REAL(*)		output	Delta times
C	NDT	INT		output	Number of delta times found
C	WORK	REAL(MAXANT,MAXANT)	input	Work array
C	MAXANT	INT		input	Size of work array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	19 October 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NPTS, NDT, MAXANT
      REAL		TIME(NPTS), BASELINE(NPTS), DT(NDT),
     $   		WORK(MAXANT,MAXANT)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISGTIP')
C
      INTEGER		I, IA1, IA2
      REAL		T, DIFF
      LOGICAL		DOMSG
C=====================================================================
      IF (ERROR) GO TO 999
C
      NDT = 0
      DOMSG = .FALSE.
      DO 100 IA1 = 1, MAXANT
         DO 100 IA2 = 1, MAXANT
            WORK(IA1,IA2) = -2.E10
 100  CONTINUE
C
      DO 500 I = 1, NPTS
         T = TIME(I)
         IA1 = NINT(BASELINE(I)) / 256
         IA2 = NINT(BASELINE(I)) - 256 * IA1
         IF ((IA1.GT.MAXANT).OR.(IA2.GT.MAXANT)) THEN
            DOMSG = .TRUE.
            GO TO 500
         END IF
         IF (WORK(IA1,IA2).LE.-1.E10) THEN
            WORK(IA1,IA2) = T
         ELSE
            DIFF = T - WORK(IA1,IA2)
            WORK(IA1,IA2) = T
            NDT = NDT + 1
            DT(NDT) = DIFF
         END IF
 500  CONTINUE
C
      IF (DOMSG) THEN
         CALL MSGPUT ('Warning: Work Buffer Exceeded in VISGTIP.',
     $      'W')
         CALL MSGPUT ('Check integration time closely','W')
      END IF
C
  999 CONTINUE
      END
