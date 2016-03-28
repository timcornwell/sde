C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrvlapb.f	1.1	 8/12/91
C
      SUBROUTINE ARRVLAPB (NPB, RADMAX, PBADD, NEW)
C
CD Make a lookup table for the VLA PB, out to first NULL ONLY
C
C
C	NPB	output	INT	Size of look up table
C	RADMAX	output	REAL	Maximum argument
C	PBADD	output	INT	Address of lookup table
C	NEW	out	L	Is it NEW?
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C
C	Only go out to first null
C	RADMAX scaled so TELDIAM comes out to 25.0 m
C				M.A.Holdaway	Aug 12 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NPB, PBADD
      REAL		RADMAX
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRVLAPB')
C
      INTEGER		DATADD
      LOGICAL		DATEXIST, NEW
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NPB = 8192
      RADMAX = 3.3063622
      IF (.NOT.DATEXIST('SCRATCH/VLAPB')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/VLAPB', 1, NPB, 'R', PBADD)
         CALL PIXVLAPB (NPB, MEMR(PBADD), RADMAX)
         NEW = .TRUE.
      ELSE
         PBADD = DATADD ('SCRATCH/VLAPB')
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
