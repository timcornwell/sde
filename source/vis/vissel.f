C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissel.f	1.2    11/7/90
C
       SUBROUTINE VISSEL (NAME, SUB, TIMERANG, UVLIMITS, NSEL)
C
CD Select visibility data according to TIMERANG and UVLIMITS.
C The selection is done by flaggin un-wanted data.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	TIMERANG	REAL(*) input	starttime, endtime
C	UVLIMITS	REAL(*)	input	umin, umax
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
       REAL		TIMERANG(*)
       REAL		UVLIMITS(*)
       INTEGER		NSEL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSEL')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD,
     1			WTADD, TADD, UADD, VADD, WADD
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
      TADD = DATADD(STRM2(NAME, 'TIME'))
C
      CALL VISSELPI (MEMR(WTADD), NAXIS(1), MEMR(TADD), MEMR(UADD),
     1   MEMR(VADD), MEMR(WADD), TIMERANG, UVLIMITS, MEMR(WTADD),
     2   NSEL)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
