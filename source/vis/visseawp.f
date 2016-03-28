C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visseawp.f	1.3	 7/20/92
C
      SUBROUTINE VISSEAWP (NVIS, BASL, WT, AUTOWT)
C
CD Change AUTOCORRELATION WT, pixel level routine
C
C	NVIS	INT	input	Number of visibilities
C	BASL	REAL(*)	input	Baselines
C	WT	REAL(*)	in/out	Input weights
C	AUTOWT	REAL	input	New AUTOCORRELATION WT
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	NOV 12 1990
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS
      REAL		BASL(*), WT(*), AUTOWT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSEAWP')
C
      INTEGER		IVIS, IA1, IA2
      LOGICAL		STRMATCH
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 100 IVIS = 1, NVIS
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
         IF (IA1 .EQ. IA2) THEN
            IF ( WT(IVIS) .GT. 0.0) THEN
               WT(IVIS) = AUTOWT
            ELSE IF ( WT(IVIS) .LT. 0.0) THEN
               WT(IVIS) = -AUTOWT
            ENDIF
         ENDIF               
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
