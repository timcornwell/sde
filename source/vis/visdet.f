C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdet.f	1.1 8/20/92
C
       SUBROUTINE VISDET (NAME, SUB, MSUB, AMP, SUMWT, CHISQ)
C
CD Detect signal in visibility data
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	SMUB	CH*(*)	input	Name of sub-class e.g. MOD/I
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB, MSUB
      REAL		SUMWT, CHISQ, AMP
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDET')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD,
     1			WTADD, VSADD, MVSADD, MWTADD, NVIS
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      CALL DATGETAR (STRM3(NAME, MSUB, 'VIS'), NAX, NAXIS, ATYPE,
     $   MVSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      MWTADD = DATADD (STRM3(NAME, MSUB, 'WT'))
C
      CALL VISDETP (MEMX(VSADD), MEMR(WTADD), MEMX(MVSADD),
     $   MEMR(MWTADD), NVIS, AMP, SUMWT, CHISQ)
      IF (ERROR) GO TO 990
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
