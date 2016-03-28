C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrgpb.f	1.3    11/7/90
C
      SUBROUTINE ARRGPB (NGPB, RADMAX, GPBADD, NEW)
C
CD Make a lookup table for the Gaussian Primary beam
C
C
C	NGPB	output	INT	Size of look up table
C	RADMAX	output	REAL	Maximum argument
C	GPBADD	output	INT	Address of lookup table
C	NEW	out	L	Is it new?
C Audit trail:
C	New routine
C				T.J.Cornwell	Jan 24 1989
C	Added "NEW"		M.A.Holdaway	Sep 11 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NGPB, GPBADD
      REAL		RADMAX
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRGPB')
C
      INTEGER		DATADD
      LOGICAL		DATEXIST, NEW
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NGPB = 16384
      RADMAX = 2.5
      IF (.NOT.DATEXIST('SCRATCH/GPB')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/GPB', 1, NGPB, 'R', GPBADD)
         CALL PIXGPB (NGPB, MEMR(GPBADD), RADMAX)
         NEW = .TRUE.
      ELSE
         GPBADD = DATADD ('SCRATCH/GPB')
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
