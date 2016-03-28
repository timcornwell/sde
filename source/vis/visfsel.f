C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visfsel.f	1.2    11/7/90
C
       SUBROUTINE VISFSEL (NAME, SUB, FSEL, NSEL)
C
CD Select visibility data according to a function FSEL which is called
C for every uv point. The selection is done by flaggin un-wanted data.
C FSEL is declared as:
C
C LOGICAL FUNCTION FSEL (U, V, W, TIME, BASL)
C
C where all arguments are REAL.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C      FSEL    LOG     input   Function
C	NSEL	INT	output	Number of vis. selected
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB
       INTEGER          NSEL
       EXTERNAL         FSEL
       LOGICAL          FSEL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISFSEL')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD,
     1			WTADD, TADD, UADD, VADD, WADD, BADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'WT'), NAX, NAXIS, ATYPE, WTADD)
      UADD = DATADD(STRM2(NAME, 'UU'))
      VADD = DATADD(STRM2(NAME, 'VV'))
      WADD = DATADD(STRM2(NAME, 'WW'))
      BADD = DATADD(STRM2(NAME, 'BASELINE'))
      TADD = DATADD(STRM2(NAME, 'TIME'))
C
      CALL VISFSELP (MEMR(WTADD), NAXIS(1), MEMR(TADD), MEMR(BADD),
     1   MEMR(UADD), MEMR(VADD), MEMR(WADD), FSEL, MEMR(WTADD),
     2   NSEL)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
