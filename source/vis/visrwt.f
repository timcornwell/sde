C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrwt.f	1.3    2/21/95
C
       SUBROUTINE VISRWT (NAME, SUB, TIMERANG, UVLIMITS,
     $   ANT, NANT, BAS, NBAS, DONEG, SCALE, OFFSET, NSEL, NTOT,
     $   RANDOM, SEED, WTTHRSH)
C
CD Reweight selected visibilities via WT' = WT * SCALE + OFFSET
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	TIMERANG REAL(*) input	starttime, endtime
C	UVLIMITS REAL(*) input	umin, umax
C	ANT	INT(*)	input	Antennas to select
C	NANT	INT	input	Number of valid elements in ANT
C	BAS	INT(*)	input	Baselines with ANT
C	NBAS	INT	input	Number of valid elements in BAS
C	DONEG	LOG	input	Can negative weights be selected?
C	SCALE	REAL	input	Scale factor for selected weights
C	OFFSET	REAL	input	Offset for selected weights
C	NSEL	INT	output	Number of vis. selected
C	NTOT	INT	output	Total number of vis examined
C	RANDOM	REAL	input	Random probability of selection 0 => 1
C	SEED	INT	input	Integer seed for the generator
C	WTTHRSH REAL	input	Weight threshold
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Mar 24 1993
C	Added DONEG option
C				D.S.Briggs	Sept 8 1993
C	Force weights negative, if WT'(i) < WTTHRSH
C				D.S.Briggs	Feb 21 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB
      REAL		TIMERANG(*), UVLIMITS(*), SCALE, OFFSET,
     $   		RANDOM, WTTHRSH
      INTEGER		NSEL, NTOT, ANT(*), BAS(*), NANT, NBAS, SEED
      LOGICAL		DONEG
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRWT')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD,
     1			WTADD, TADD, UADD, VADD, WADD, BADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'WT'), NAX, NAXIS, ATYPE, WTADD)
      NTOT = NAXIS(1)
      UADD = DATADD(STRM2(NAME, 'UU'))
      VADD = DATADD(STRM2(NAME, 'VV'))
      WADD = DATADD(STRM2(NAME, 'WW'))
      TADD = DATADD(STRM2(NAME, 'TIME'))
      BADD = DATADD(STRM2(NAME, 'BASELINE'))
C
      CALL VISRWTP (MEMR(WTADD), NTOT, MEMR(TADD), MEMR(BADD),
     $   MEMR(UADD), MEMR(VADD), MEMR(WADD), TIMERANG, UVLIMITS,
     $   ANT, NANT, BAS, NBAS, DONEG, SCALE, OFFSET, RANDOM, SEED,
     $   WTTHRSH, MEMR(WTADD), NSEL)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
