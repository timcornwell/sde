C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrairy.f	1.3    11/7/90
C
      SUBROUTINE ARRAIRY (NAIRY, RADMAX, AIRYADD, NEW)
C
CD Make a lookup table for the Airy disk
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
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NAIRY, AIRYADD
      REAL		RADMAX
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRAIRY')
C
      INTEGER		DATADD
      LOGICAL		DATEXIST, NEW
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NAIRY = 16384
      RADMAX = 7.016
      IF (.NOT.DATEXIST('SCRATCH/AIRY')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/AIRY', 1, NAIRY, 'R', AIRYADD)
         CALL PIXAIRY (NAIRY, MEMR(AIRYADD), RADMAX)
         NEW = .TRUE.
      ELSE
         AIRYADD = DATADD ('SCRATCH/AIRY')
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
