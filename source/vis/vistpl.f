C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vistpl.f	1.3	 12/22/92
C
       SUBROUTINE VISTPL (NAME, SUB, DEVPH, DEVAM, ANT1, ANT2, DT, 
     $   STOKES, STYLE)
C
CD Plot visibilities vs time
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	DEVPH	CH*(*)	input	Praphics device, phase
C	DEVAM	CH*(*)	input	Praphics device, amp
C	ANT1	INT	input	Antenna 1
C	ANT2	INT	input	Antenna 2
C	DT	REAL	input	Averaging time, seconds
C	STOKES	CH*(*)	input	Stokes for plot label
C	STYLE	INT	input	Style for characters in plot
C	
C
C Audit trail:
C	New routine
C				M.A.Holdaway	Oct 23 1991
C
C	Added  STOKES to plot label
C				M.A.Holdaway	22 DEC 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, DEVPH, DEVAM, STOKES
       INTEGER		ANT1, ANT2, STYLE
       REAL		DT
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISTPL')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD, 
     1			BADD, WTADD, TADD, VSADD, NVIS,
     3			DTADD, DAADD, DPADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      TADD = DATADD (STRM2(NAME, 'TIME'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
C
      CALL DATMAKAR ('DummyTime', 1, NVIS, 'R', DTADD)
      CALL DATMAKAR ('DummyAmp', 1, NVIS, 'R', DAADD)
      CALL DATMAKAR ('DummyPhase', 1, NVIS, 'R', DPADD)
C
      CALL VISTPLP (MEMX(VSADD), DEVPH, DEVAM, MEMR(BADD), 
     $   MEMR(TADD), MEMR(WTADD), MEMR(DTADD), MEMR(DAADD), MEMR(DPADD),
     $   ANT1, ANT2, NVIS, STOKES, STYLE)
C
      CALL DATDELET ('DummyTime')
      CALL DATDELET ('DummyAmp')
      CALL DATDELET ('DummyPhase')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
