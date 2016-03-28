C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vislistb.f	1.2    6/5/93
C
       SUBROUTINE VISLISTB (HANDLE, NAME, SUB, NLIST, DOALL)
C
CD List visibility data  in order of baseline
C
C	HANDLE	CH*(*)	input	HANDLE of output stream
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	NLIST	INT	input	Maximum number of records to list
C	DOALL	LOG	input	Use weights?
C
C Audit trail:
C	New routine
C				T.J.Cornwell	Sep 19 1991
C	NLIST & DOALL option added
C				D.S.Briggs	25 May 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB, HANDLE
      INTEGER		NLIST
      LOGICAL		DOALL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISLISTB')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD, GADD,
     1			BADD, WTADD, TADD, VSADD, UADD, VADD, NVIS,
     3			NDUMMY
      CHARACTER*1	ATYPE
C
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      LOGICAL		DATEXIST
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      UADD = DATADD (STRM2(NAME, 'UU'))
      VADD = DATADD (STRM2(NAME, 'VV'))
      TADD = DATADD (STRM2(NAME, 'TIME'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
C
      CALL VISLISBP (HANDLE, MEMX(VSADD), MEMR(BADD), MEMR(TADD), 
     1   MEMR(WTADD), MEMR(UADD), MEMR(VADD), NVIS, NLIST, DOALL)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
