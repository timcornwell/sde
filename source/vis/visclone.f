C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visclone.f	1.2    11/7/90
C
       SUBROUTINE VISCLONE (NAME, SUB, STOKES, OUTSUB)
C
CD Clone visibility data
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS
C	OUTSUB	CH*(*)	input	Name of outclass e.g. MOD
C Audit trail:
C      Minor change in order
C				T.J.Cornwell	April 4 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, OUTSUB, STOKES
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCLONE')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), VSADD
      REAL		RPIX (SYSMXDIM), DELT(SYSMXDIM),
     1			ROTA (SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3, SUBS, OUTSUBS
      LOGICAL	DATEXIST
C=======================================================================
C
      IF (ERROR) GO TO 999
C
      SUBS = STRM2 (SUB, STOKES)
      OUTSUBS = STRM2 (OUTSUB, STOKES)
      CALL DATCREAT (STRM2(NAME, OUTSUB))
      CALL DATCREAT (STRM2(NAME, OUTSUBS))
C
      IF (.NOT.DATEXIST(STRM3(NAME, OUTSUBS, 'VIS'))) THEN
         CALL DATGETAR (STRM3(NAME, SUBS, 'VIS'), NAX, NAXIS, ATYPE, 
     1      VSADD)
         CALL DATMAKAR (STRM3(NAME, OUTSUBS, 'VIS'), NAX, NAXIS, ATYPE, 
     1      VSADD)
         CALL ARRCOPY (STRM3(NAME, SUBS, 'VIS'), 
     1      STRM3(NAME, OUTSUBS, 'VIS'))
      END IF      
C
      IF (.NOT.DATEXIST(STRM3(NAME, OUTSUBS, 'WT'))) THEN
         CALL DATGETAR (STRM3(NAME, SUBS, 'WT'), NAX, NAXIS, ATYPE, 
     1      VSADD)
         CALL DATMAKAR (STRM3(NAME, OUTSUBS, 'WT'), NAX, NAXIS, ATYPE, 
     1      VSADD)
         CALL ARRCOPY (STRM3(NAME, SUBS, 'WT'), 
     1      STRM3(NAME, OUTSUBS, 'WT'))
      END IF
C
      CALL CRDGET (STRM2(NAME, SUBS), NAX, TYPE, NAXIS, RVAL, 
     1   RPIX, DELT, ROTA)
      CALL CRDPUT (STRM2(NAME, OUTSUBS), NAX, TYPE, NAXIS, RVAL, 
     1   RPIX, DELT, ROTA)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
