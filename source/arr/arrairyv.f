C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrairyv.f	1.3    11/7/90
C
      SUBROUTINE ARRAIRYV (NAIRY, RADMAX, AIRYADD)
C
CD Make a lookup table for the Airy disk
C
C
C	NAIRY	output	INT	Size of look up table
C	RADMAX	output	REAL	Maximum argument
C	AIRYADD	output	INT	Address of lookup table
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Altered for antenna VOLTAGE pattern
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

      PARAMETER		(ROUTINE = 'ARRAIRYV')
C
      INTEGER		DATADD
      LOGICAL		DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NAIRY = 16384
      RADMAX = 7.016
      IF (.NOT.DATEXIST('SCRATCH/AIRYV')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/AIRYV', 1, NAIRY, 'R', AIRYADD)
         CALL PIXAIRYV (NAIRY, MEMR(AIRYADD), RADMAX)
      ELSE
         AIRYADD = DATADD ('SCRATCH/AIRYV')
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
