C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visshift.f	1.3    3/8/91
C
       SUBROUTINE VISSHIFT (NAME, SUB, SHIFT, SUBO)
C
CD Phase-rotate to acheive a shift in position
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	SHIFT	REAL	input	Shift in arcseconds
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Too many arguments in call to VISSHIFP until now
C				M.A.Holdaway	Feb 13 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, SUBO
       REAL		SHIFT(*)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSHIFT')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD, VSADD,
     1			WTADD, VSOADD, UADD, VADD, WADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, 
     1   VSADD)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      VSOADD = DATADD (STRM3(NAME, SUBO, 'VIS'))
      UADD = DATADD(STRM2(NAME, 'UU'))
      VADD = DATADD(STRM2(NAME, 'VV'))
      WADD = DATADD(STRM2(NAME, 'WW'))
C
      CALL VISSHIFP (MEMX(VSADD), MEMR(WTADD), NAXIS(1), 
     1   MEMR(UADD), MEMR(VADD), MEMR(WADD), SHIFT, MEMX(VSOADD))
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
