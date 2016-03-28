C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visnorm.f	1.1 9/29/92
C
       SUBROUTINE VISNORM (NAME, SUB, MSUB, FACTOR)
C
CD Return normalization factor between model and data
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	SMUB	CH*(*)	input	Name of sub-class e.g. MOD/I
C	FACTOR	REAL	output	OBS/MODEL
C	MODE	CH*(*)	input	' '|'AMPPHI' | 'GLOBAL'
C Audit trail:
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, MSUB
       REAL		FACTOR
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISNORM')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD,
     1			WTADD, VSADD, MVSADD, MWTADD, NVIS
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      LOGICAL		DATEXIST
C=======================================================================
      FACTOR = 1.0
C
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      MVSADD = DATADD (STRM3(NAME, MSUB, 'VIS'))
      MWTADD = DATADD (STRM3(NAME, MSUB, 'WT'))
C
      CALL VISNORMP (MEMX(VSADD), MEMX(MVSADD), 
     1   MEMR(WTADD), MEMR(MWTADD), NVIS, FACTOR)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
