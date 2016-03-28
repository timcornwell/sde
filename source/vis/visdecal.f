C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdecal.f	1.1 26 Jan 1995
C
       SUBROUTINE VISDECAL (NAME, SUB, MOD, NANT, DERR, A2)
C
CD Calibrate data
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	FLUX	REAL	input	Estimated flux
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, MOD
       INTEGER		NANT
       REAL		A2, DERR(NANT)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDECAL')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD,
     1			BADD, WTADD, VSADD, MVSADD, MWTADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      MVSADD = DATADD (STRM3(NAME, MOD, 'VIS'))
      MWTADD = DATADD (STRM3(NAME, MOD, 'WT'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
C
      CALL VISDECAP (MEMX(VSADD), MEMR(BADD), MEMR(WTADD),
     $   MEMX(MVSADD), MEMR(MWTADD), NAXIS(1), 
     1   NANT, DERR, A2)
      IF (ERROR) GO TO 990
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
