C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissub.f	1.2    11/7/90
C
      SUBROUTINE VISSUB (IN, SUB, STOKES, OUT)
C
CD Find valid subset of visibility data
C
C
C	IN	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Class
C	OUT	CH*(*)	input	Name of output visibility set
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IN, SUB, OUT, STOKES
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'VISSUB')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NSEL, ADD, NADD,
     1			DATADD, WTADD, N
      REAL		RPIX (SYSMXDIM), DELT(SYSMXDIM),
     1			ROTA (SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3, SSUB
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATCREAT(OUT)
      CALL DATCREAT(STRM2(OUT, SUB))
      CALL DATCREAT(STRM3(OUT, SUB, STOKES))
C
C Copy coordinate items
C
      CALL CRDGET (STRM3(IN, SUB, STOKES), NAX, TYPE, NAXIS, 
     1   RVAL, RPIX, DELT, ROTA)
      CALL CRDPUT(STRM3(OUT, SUB, STOKES), NAX, TYPE, NAXIS, 
     1   RVAL, RPIX, DELT, ROTA)
C
C Copy header items
C
      CALL HEDCOPY (IN, OUT)
C
C Now copy arrays, condensing as required
C
      SSUB = STRM2(SUB, STOKES)
      CALL DATGETAR (STRM3(IN, SSUB, 'WT'), NAX, NAXIS, ATYPE, WTADD)
      N = NAXIS(1)
      CALL PIXRVALI (MEMR(WTADD), N, NSEL)
      NAXIS(1) = NSEL
      CALL DATMAKAR (STRM3(OUT, SSUB, 'WT'), NAX, NAXIS, 'R', NADD)
      CALL PIXRSEL (MEMR(WTADD), MEMR(WTADD), N, MEMR(NADD), NSEL)
      ADD = DATADD (STRM3(IN, SSUB, 'VIS'))
      CALL DATMAKAR (STRM3(OUT, SSUB, 'VIS'), NAX, NAXIS, 'X', NADD)
      CALL PIXXSEL (MEMX(ADD), MEMR(WTADD), N, MEMX(NADD), NSEL)
      ADD = DATADD (STRM2(IN, 'UU'))
      CALL DATMAKAR (STRM2(OUT, 'UU'), NAX, NAXIS, 'R', NADD)
      CALL PIXRSEL (MEMR(ADD), MEMR(WTADD), N, MEMR(NADD), NSEL)
      ADD = DATADD (STRM2(IN, 'VV'))
      CALL DATMAKAR (STRM2(OUT, 'VV'), NAX, NAXIS, 'R', NADD)
      CALL PIXRSEL (MEMR(ADD), MEMR(WTADD), N, MEMR(NADD), NSEL)
      ADD = DATADD (STRM2(IN, 'WW'))
      CALL DATMAKAR (STRM2(OUT, 'WW'), NAX, NAXIS, 'R', NADD)
      CALL PIXRSEL (MEMR(ADD), MEMR(WTADD), N, MEMR(NADD), NSEL)
      ADD = DATADD (STRM2(IN, 'TIME'))
      CALL DATMAKAR (STRM2(OUT, 'TIME'), NAX, NAXIS, 'R', NADD)
      CALL PIXRSEL (MEMR(ADD), MEMR(WTADD), N, MEMR(NADD), NSEL)
      ADD = DATADD (STRM2(IN, 'BASELINE'))
      CALL DATMAKAR (STRM2(OUT, 'BASELINE'), NAX, NAXIS, 'R', NADD)
      CALL PIXRSEL (MEMR(ADD), MEMR(WTADD), N, MEMR(NADD), NSEL)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
