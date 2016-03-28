C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrgvp.f	1.3    11/7/90
C
      SUBROUTINE ARRGVP (NGPB, RADMAX, GPBADD)
C
CD Make a lookup table for the Gaussian Voltage Pattern
C
C
C	NGPB	output	INT	Size of look up table
C	RADMAX	output	REAL	Maximum argument
C	GPBADD	output	INT	Address of lookup table
C Audit trail:
C	New routine
C				T.J.Cornwell	Jan 24 1989
C	Voltage Pattern
C				M.A.Holdaway	Feb 13 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NGPB, GPBADD
      REAL		RADMAX
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRGVP')
C
      INTEGER		DATADD
      LOGICAL		DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NGPB = 16384
      RADMAX = 2.5
      IF (.NOT.DATEXIST('SCRATCH/GVP')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/GVP', 1, NGPB, 'R', GPBADD)
         CALL PIXGVP (NGPB, MEMR(GPBADD), RADMAX)
      ELSE
         GPBADD = DATADD ('SCRATCH/GVP')
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


