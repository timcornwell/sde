C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vismxant.f	1.1    12/27/91
C
      SUBROUTINE VISMXANT (NAME, MINANT, MAXANT)
C
CD Find the min, max antenna number in a database.
C
C	NAME	CH*(*)	input	Name of directory entry
C	MINANT	INT	output	Minimum antenna number in db
C	MAXANT	INT	output	Maximum antenna number in db
C
C If an evaluation of VISSTAT0 is required, a number of useful min/max
C quantities are stored in NAME/STAT/... for future reference.
C
C Audit trail:
C	Original version: Cloned from VISGTINT
C				D.S.Briggs	28-Oct-91
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
      INTEGER		MINANT, MAXANT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISMXANT')
C
      INTEGER		DATFGETI
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)      STRM2
C=======================================================================
      IF (ERROR) GO TO 999
C
C Assume that anything found in the DB is correct
C
      IF (.NOT. (DATEXIST(STRM2(NAME,'STAT/MINANT')).AND.
     $           DATEXIST(STRM2(NAME,'STAT/MAXANT')))) THEN
C
C Do full min/max scan (without flagging)
C
         CALL VISSTAT1 (NAME, STRM2(NAME,'STAT'))
      END IF
C
      MINANT = DATFGETI(STRM2(NAME,'STAT'),'MINANT')
      MAXANT = DATFGETI(STRM2(NAME,'STAT'),'MAXANT')
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
