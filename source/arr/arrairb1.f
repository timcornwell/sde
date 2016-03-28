C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrairb1.f	1.1	 9/10/91
C
      SUBROUTINE ARRAIRB1 (NAIRY, RADMAX, AIRYADD, NEW)
C
CD Make a lookup table for a blocked aperture "Airy" disk
C Blocking is fixed at 1/10 the dish diameter.
C
C
C	NAIRY	output	INT	Size of look up table
C	RADMAX	output	REAL	Maximum argument
C	AIRYADD	output	INT	Address of lookup table
C	NEW	out	L	Is it NEW?
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added "NEW"		M.A.Holdaway	Sep 11 1990
C	This version only goes to First Null
C				M.A.Holdaway	Sep 10 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NAIRY, AIRYADD
      REAL		RADMAX
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRAIRB1')
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
      IF (.NOT.DATEXIST('SCRATCH/AIRYB1')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/AIRYB1', 1, NAIRY, 'R', AIRYADD)
         CALL PIXAIRYB (NAIRY, MEMR(AIRYADD), RADMAX)
         NEW = .TRUE.
      ELSE
         AIRYADD = DATADD ('SCRATCH/AIRYB1')
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
