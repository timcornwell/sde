C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visnant.f	1.2    11/7/90
C
      SUBROUTINE VISNANT(NAME, NANT)
C
CD Find the maximum antenna number in a visibility data base
C
C
C	NAME	CH*(*)	input	Name of directory entry
C      NANT    INT     output  Maximum antenna number
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	April 4 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
      INTEGER           NANT
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'VISNANT')
C
      INTEGER           BADD, IVIS, IA1, IA2, NAX, NAXIS(SYSMXDIM)
      CHARACTER*1       ATYPE
      CHARACTER*(SYSMXNAM)      STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      NANT = 0
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM2(NAME, 'BASELINE'), NAX, NAXIS, ATYPE, BADD)
      IF (ERROR) GO TO 990
C
      NANT = 1
      DO 10 IVIS = 1, NAXIS(1)
         IA1 = NINT(MEMR(BADD+IVIS-1)/256.0)
         IA2 = NINT(MEMR(BADD+IVIS-1)-FLOAT(256*IA1))
         NANT = MAX (NANT, IA1, IA2)
 10   CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
