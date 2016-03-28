C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calsflis.f	1.4    5/15/92
C
      SUBROUTINE CALSFLIS (HANDLE, NAME, SUB)
C
CD Print antenna gain structure function
C
C
C	HANDLE	CH*(*)	input	Handle for printing
C	NAME	CH*(*)	input	Name of visibility file
C	SUB	CH*(*)	input	Name of subclass e.g. 'OBS/I'
C Audit trail:
C	New version
C				T.J.Cornwell	Jan 16 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB, HANDLE
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'CALSFLIS')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NUMINT, NANT
      INTEGER		SFADD, I, IANT, DATADD
      INTEGER		NDUMMY
      INTEGER		IA1,IA2, ILAG, NLAG
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      CHARACTER*12	STRTIMC, DSTRING
      CHARACTER*1	ATYPE
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C
      CALL DATGETAR (STRM3(NAME, SUB, 'ANTGAINSF'), NAX, NAXIS, 
     1   ATYPE, SFADD)
      NANT = NAXIS(1)
      NLAG = NAXIS(3)
C
      CALL CALSFLIP (HANDLE, MEMR(SFADD), NANT, NLAG)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
