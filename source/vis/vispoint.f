C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vispoint.f	1.2    11/7/90
C
       SUBROUTINE VISPOINT (NAME, SUB, POINT, FLUX)
C
CD Set visibility to point source
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	FLUX	REAL	input	Estimated flux
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, POINT
       REAL		FLUX
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPOINT')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD,
     1			WTADD, VSADD, NWTADD
      REAL		RPIX (SYSMXDIM), DELT(SYSMXDIM),
     1			ROTA (SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
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
      CALL DATMAKAR (STRM3(NAME, POINT, 'VIS'), NAX, NAXIS, 'X',
     1   VSADD)
      CALL DATMAKAR (STRM3(NAME, POINT, 'WT'), NAX, NAXIS, 'R',
     1   NWTADD)
C
      CALL VISPOINP (MEMX(VSADD), MEMR(WTADD), NAXIS(1), 
     1   FLUX, MEMR(NWTADD))
      IF (ERROR) GO TO 990
C
      CALL CRDGET (STRM2(NAME, SUB), NAX, TYPE, NAXIS, RVAL, 
     1   RPIX, DELT, ROTA)
      CALL CRDPUT (STRM2(NAME, POINT), NAX, TYPE, NAXIS, RVAL, 
     1   RPIX, DELT, ROTA)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
