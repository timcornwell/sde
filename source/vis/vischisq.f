C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vischisq.f	1.2	 9/14/92
C
       SUBROUTINE VISCHISQ (NAME, SUB, MSUB, CHISQ, SUMWT, SUMWTS, NU)
C
CD Calculate Chi^2 wrt model
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	SMUB	CH*(*)	input	Name of sub-class e.g. MOD/I
C	CHISQ	REAL	output	Reduced Chi^2
C	NU	INT	output	Degrees of freedom
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	6 July 1992
C	Changed to return more useful info
C				T.J.Cornwell	Sept 14 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, MSUB
       REAL		CHISQ, SUMWT, SUMWTS
       INTEGER		NU
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISEDIT')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NVIS,
     $   		VSADD, WTADD, MVSADD
      CHARACTER*1	ATYPE
C
      CHARACTER*(SYSMXNAM)	STRM3
      INTEGER		DATADD
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      MVSADD = DATADD (STRM3(NAME, MSUB, 'VIS'))
C
      CALL VISCHSQP (MEMX(VSADD), MEMX(MVSADD), MEMR(WTADD),
     $   NVIS, CHISQ, SUMWT, SUMWTS, NU)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
