C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrairy1.f	1.1	 5/17/91
C
      SUBROUTINE ARRAIRY1 (NAIRY, RADMAX, AIRYADD, NEW)
C
CD Make a lookup table for the Airy disk, out to first NULL ONLY
C
C
C	NAIRY	output	INT	Size of look up table
C	RADMAX	output	REAL	Maximum argument
C	AIRYADD	output	INT	Address of lookup table
C	NEW	out	L	Is it NEW?
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C
C	Only go out to first null
C				M.A.Holdaway	May 17 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NAIRY, AIRYADD
      REAL		RADMAX
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRAIRY1')
C
      INTEGER		DATADD
      LOGICAL		DATEXIST, NEW
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NAIRY = 8192
      RADMAX = 3.833
      IF (.NOT.DATEXIST('SCRATCH/AIRY1')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/AIRY1', 1, NAIRY, 'R', AIRYADD)
         CALL PIXAIRY (NAIRY, MEMR(AIRYADD), RADMAX)
         NEW = .TRUE.
      ELSE
         AIRYADD = DATADD ('SCRATCH/AIRY1')
         NEW = .FALSE.
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
