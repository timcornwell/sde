C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visclone.f	1.2    11/7/90
C
       SUBROUTINE VISSCLON (NAME, STOKIN, STOKOUT)
C
CD Clone visibility data from one STOKES to another STOKES
C
C
C	NAME	CH*(*)	input	Name of sub-directory entry ('Vis/OBS')
C	STOKIN	CH*(1)	input	Stokes to clone FROM (IQUV)
C	STOKOUT	CH*(1)	input	Stokes to clone TO (IQUV)
C
C Audit trail:
C      Cloned from VISCLONE
C				M.A.Holdaway	Sept 9 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, STOKIN, STOKOUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSCLON')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), VSADD
      REAL		RPIX (SYSMXDIM), DELT(SYSMXDIM),
     1			ROTA (SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, INSUBS, OUTSUBS
      LOGICAL	DATEXIST
C=======================================================================
C
      IF (ERROR) GO TO 999
C
      INSUBS  = STRM2 (NAME, STOKIN)
      OUTSUBS = STRM2 (NAME, STOKOUT)
      CALL DATCREAT (OUTSUBS)
C
      IF (.NOT.DATEXIST(STRM2(OUTSUBS, 'VIS'))) THEN
         CALL DATGETAR (STRM2(INSUBS,  'VIS'), NAX, NAXIS, ATYPE, 
     1      VSADD)
         CALL DATMAKAR (STRM2(OUTSUBS,  'VIS'), NAX, NAXIS, ATYPE, 
     1      VSADD)
         CALL ARRCOPY (STRM2(INSUBS, 'VIS'), STRM2(OUTSUBS, 'VIS'))
      END IF      
C
      IF (.NOT.DATEXIST(STRM2(OUTSUBS, 'WT'))) THEN
         CALL DATGETAR (STRM2(INSUBS,  'WT'), NAX, NAXIS, ATYPE, 
     1      VSADD)
         CALL DATMAKAR (STRM2(OUTSUBS,  'WT'), NAX, NAXIS, ATYPE, 
     1      VSADD)
         CALL ARRCOPY (STRM2(INSUBS, 'WT'), STRM2(OUTSUBS, 'WT'))
      END IF      
C
      CALL CRDGET (INSUBS, NAX, TYPE, NAXIS, RVAL, 
     1   RPIX, DELT, ROTA)
      CALL CRDPUT (OUTSUBS, NAX, TYPE, NAXIS, RVAL, 
     1   RPIX, DELT, ROTA)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
