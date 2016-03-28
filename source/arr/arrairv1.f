C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrairv1.f	1.1	 5/17/91
C
      SUBROUTINE ARRAIRV1 (NAIRY, RADMAX, AIRYADD)
C
CD Make a lookup table for a "Airy" disk VOLTAGE pattern
C
C
C	NAIRY	output	INT	Size of look up table
C	RADMAX	output	REAL	Maximum argument
C	AIRYADD	output	INT	Address of lookup table
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C
C	Only go out to FIRST NULL
C				M.A.Holdaway	Sep 12 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NAIRY, AIRYADD
      REAL		RADMAX
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRAIRV1')
C
      INTEGER		DATADD
      LOGICAL		DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NAIRY = 8192
      RADMAX = 3.833
      IF (.NOT.DATEXIST('SCRATCH/AIRYV1')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/AIRYV1', 1, NAIRY, 'R', AIRYADD)
         CALL PIXAIRV (NAIRY, MEMR(AIRYADD), RADMAX)
      ELSE
         AIRYADD = DATADD ('SCRATCH/AIRYV1')
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
