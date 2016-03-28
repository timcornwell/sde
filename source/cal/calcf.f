C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calcf.f	1.4    5/15/92
C
      SUBROUTINE CALCF (NAME, SUB)
C
CD Find correlation of antenna gains: this is stored as a 3-D complex
C array called ANTGAINCF
C
C
C	NAME	CH*(*)	input	Name of visibility file
C	SUB	CH*(*)	input	Name of subclass e.g. 'OBS/I'
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'CALCF')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NUMINT, NANT
      INTEGER		GADD, TGADD, I, IANT, ORADD, NRADD, DATADD
      INTEGER		OFFSET, CFADD, UADD, VADD, ISTAT, NDUMMY
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      CHARACTER*12	STRTIMC, DSTRING
      CHARACTER*1	ATYPE
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM3(NAME, SUB, 'ANTGAIN'), NAX, NAXIS, 
     1   ATYPE, GADD)
      NANT = NAXIS(1)
      NUMINT = NAXIS(2)
      TGADD = DATADD(STRM3(NAME, SUB, 'GAINTIME'))
      ORADD = DATADD(STRM3(NAME, SUB, 'ORES'))
      NRADD = DATADD(STRM3(NAME, SUB, 'NRES'))
C      UADD = DATADD(STRM3(NAME, SUB, 'UPOS'))
C      VADD = DATADD(STRM3(NAME, SUB, 'VPOS'))
      IF (ERROR) GO TO 990
C
      NAX = 3
      NAXIS(1) = NANT
      NAXIS(2) = NANT
      NAXIS(3) = 4
      CALL DATMAKAR (STRM3(NAME, SUB, 'ANTGAINCF'), NAX, NAXIS,
     1   'X', CFADD)
      CALL CALCFPIX(MEMX(GADD), NANT, NUMINT, MEMX(CFADD), NAXIS(3))
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
