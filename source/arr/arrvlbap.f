C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrvlbap.f	1.3    11/7/90
C
      SUBROUTINE ARRVLBAP (NVLBA, RADMAX, VLBAADD, NEW)
C
CD Make a lookup table for a VLBA aperture disk
C
C	NVLBA	output	INT	Size of look up table
C	RADMAX	output	REAL	Maximum argument
C	VLBAADD	output	INT	Address of lookup table
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added "NEW"		M.A.Holdaway	Sep 11 1990
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NVLBA, VLBAADD
      REAL		RADMAX
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRVLBAP')
C
      INTEGER		DATADD
      LOGICAL		DATEXIST, NEW
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NVLBA = 16384
      RADMAX = 7.016
      IF (.NOT.DATEXIST('SCRATCH/VLBAP')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/VLBAP', 1, NVLBA, 'R', VLBAADD)
         CALL PIXVLBAP (NVLBA, MEMR(VLBAADD), RADMAX)
         NEW = .TRUE.
      ELSE
         VLBAADD = DATADD ('SCRATCH/VLBAP')
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
