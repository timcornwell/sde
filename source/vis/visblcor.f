C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visblcor.f	1.2 22 Feb 1995
C
       SUBROUTINE VISBLCOR (NAME, SUB, CAL, NANT, CORGAIN)
C
CD Calibrate data
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	FLUX	REAL	input	Estimated flux
C	CAL	CH*(*)	input	Name of calibrated data
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, CAL
       INTEGER		NANT
       COMPLEX		CORGAIN(NANT, *)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISBLCOR')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD,
     1			BADD, WTADD, VSADD, VSNADD, WTNADD, NFLAG
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      VSNADD = DATADD (STRM3(NAME, CAL, 'VIS'))
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      WTNADD = DATADD (STRM3(NAME, CAL, 'WT'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
C
      CALL VISBLCOP (MEMX(VSADD), MEMR(BADD), MEMR(WTADD), NAXIS(1), 
     1   NANT, CORGAIN, MEMX(VSNADD), MEMR(WTNADD), NFLAG)
      IF (ERROR) GO TO 990
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
