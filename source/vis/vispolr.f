C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vispolr.f	1.1	 7/21/94
C
      SUBROUTINE VISPOLR (VIS, CHI)
C
CD  Remove polarized part of visibilities from I
C
C	VIS	CH*(*)	inp	Visibility Database
C	CHI	CH*(*)	inp	Chi Database
C
C Audit trail:
C	New
C				M.A.Holdaway	Jul 21 1994
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS, CHI
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPOLR')
C
      INTEGER		NVIS
      INTEGER		IVSADD, IWTADD
      INTEGER		QVSADD, QWTADD
      INTEGER		UVSADD, UWTADD
      INTEGER		TADD, BADD
      CHARACTER*1	ATYPE
      INTEGER		NAX, NAXIS(SYSMXDIM)
C
      INTEGER			DATADD
      INTEGER			CHICADD, CHITADD, NCHI
      CHARACTER*(SYSMXNAM)	STRM3, STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM3(VIS, 'OBS', 'I/VIS'), NAX, NAXIS, ATYPE, 
     $   IVSADD)
      NVIS = NAXIS(1)
      IWTADD  = DATADD (STRM3(VIS, 'OBS', 'I/WT'))
C
      QVSADD  = DATADD (STRM3(VIS, 'OBS', 'Q/VIS'))
      QWTADD  = DATADD (STRM3(VIS, 'OBS', 'Q/WT'))
C
      UVSADD  = DATADD (STRM3(VIS, 'OBS', 'U/VIS'))
      UWTADD  = DATADD (STRM3(VIS, 'OBS', 'U/WT'))
C
      TADD = DATADD (STRM2(VIS, 'TIME'))
      BADD = DATADD (STRM2(VIS, 'BASELINE'))
C
C Get the CHI information
C
      CALL DATGETAR (STRM2(CHI, 'TIME'), NAX, NAXIS, ATYPE, 
     $   CHITADD)
      CALL DATGETAR (STRM2(CHI, 'CHI'), NAX, NAXIS, ATYPE, 
     $   CHICADD)
      NCHI = NAXIS(1)
C
      CALL VISPOLRP (MEMX(IVSADD), MEMR(IWTADD), 
     $   MEMX(QVSADD), MEMR(QWTADD), 
     $   MEMX(UVSADD), MEMR(UWTADD), 
     $   MEMR(TADD), NVIS, 
     $   MEMR(CHITADD), MEMR(CHICADD), NCHI)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



