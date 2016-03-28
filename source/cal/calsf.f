C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calsf.f	1.4    5/15/92
C
      SUBROUTINE CALSF (NAME, SUB, NLAG, SFTYPE)
C
CD Find structure fn of antenna gains: this is stored as a 3-D complex
C array called ANTGAINCF
C
C
C	NAME	CH*(*)	input	Name of visibility file
C	SUB	CH*(*)	input	Name of subclass e.g. 'OBS/I'
C	NLAG	INT	input	Number of lags to estimate
C	SFTYPE	CH*(*)	input	Type of structure function
C				'AMP'|'PHASE'|'AMPPHASE'
C Audit trail:
C	Changed to ignore even/odd distinction
C				T.J. Cornwell	Jan 11 1989
C	Changed to add SFTYPE
C				T.J. Cornwell	Jan 12 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB, SFTYPE
      INTEGER		NLAG
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'CALSF')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NUMINT, NANT
      INTEGER		GADD, I, IANT, DATADD
      INTEGER		OFFSET, SFADD, UADD, VADD, ISTAT, NDUMMY
      INTEGER		NUMSFADD
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
C
C Make arrays
C
      NAX = 3
      NAXIS(1) = NANT
      NAXIS(2) = NANT
      NAXIS(3) = NLAG
      CALL DATMAKAR (STRM3(NAME, SUB, 'ANTGAINSF'), NAX, NAXIS,
     1   'R', SFADD)
      CALL DATMAKAR (STRM3(NAME, SUB, 'ANTGAINNUMSF'), NAX, NAXIS,
     1   'I', NUMSFADD)
C
C Call routine which does the work
C
      CALL CALSFPIX(MEMX(GADD), NANT, NUMINT, MEMR(SFADD), 
     1   MEMI(NUMSFADD), NLAG, SFTYPE)
      CALL DATDELET (STRM3(NAME, SUB, 'ANTGAINNUMSF'))
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
