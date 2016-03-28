C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visstat1.f	1.1    12/28/91
C
       SUBROUTINE VISSTAT1 (NAME, STAT)
C
CD Find min/max of various quantities in a visibility data set.
C
C This is very similar to VISSTAT0, but only deals with SUBCLASS invariant
C quantities, and ignores flagging information.
C
C	NAME	CH*(*)	input	Name of visibility directory
C       SUB     CH*(*)	input	Name of visibility subclass
C	STAT	CH*(*)	input	Name of statistics directory
C	STAT/MINANT	INT	output	Minimum antenna number
C	STAT/MAXANT	INT	output	Maximum antenna number
C	STAT/MINUV	REAL	output	Minimum uv radius
C	STAT/MAXUV	REAL	output  Maximum uv radius
C	STAT/MINUVW	REAL	output	Minimum uvw radius
C	STAT/MAXUVW	REAL	output	Maximum uvw radius
C	STAT/MINUU	REAL	output	Minimum uu
C	STAT/MAXUU	REAL	output	Maximum uu
C	STAT/MINVV	REAL	output	Minimum vv
C	STAT/MAXVV	REAL	output	Maximum vv
C	STAT/MINWW	REAL	output	Minimum ww
C	STAT/MAXWW	REAL	output	Maximum ww
C	STAT/MINTIME	REAL	output	Minimum time
C	STAT/MAXTIME	REAL	output	Maximum time
C
C Audit trail:
C	New Routine
C				D.S.Briggs	Sept 7 1991
C	Cloned from VISSTAT0
C				D.S.Briggs	Oct 28 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, STAT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSTAT1')
C
      INTEGER		MINANT, MAXANT
      REAL		MINUV, MAXUV, MINUVW, MAXUVW, MINUU, MAXUU,
     $   		MINVV, MAXVV, MINWW, MAXWW, MINTIME, MAXTIME
C
      INTEGER		NAX, NAXIS(SYSMXDIM), TADD,
     $			BADD, UADD, VADD, WADD
      CHARACTER		ATYPE*1
C
      INTEGER		DATADD
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM2(NAME, 'UU'), NAX, NAXIS, ATYPE, UADD)
      VADD = DATADD(STRM2(NAME, 'VV'))
      WADD = DATADD(STRM2(NAME, 'WW'))
      TADD = DATADD(STRM2(NAME, 'TIME'))
      BADD = DATADD(STRM2(NAME, 'BASELINE'))
      IF (ERROR) GO TO 990
C
      CALL VISSTA1P (MEMR(TADD), MEMR(BADD), MEMR(UADD), MEMR(VADD),
     $   MEMR(WADD), NAXIS(1), MINANT, MAXANT, MINUV, MAXUV, MINUVW,
     $   MAXUVW, MINUU, MAXUU, MINVV, MAXVV, MINWW, MAXWW, MINTIME,
     $   MAXTIME)
      IF (ERROR) GO TO 990
C
C Stuff all of the stats into the output directory
C
      IF (.NOT.DATEXIST(STAT)) CALL DATCREAT (STAT)
      CALL DATPUTI (STAT, 'MINANT', MINANT, 1)
      CALL DATPUTI (STAT, 'MAXANT', MAXANT, 1)
      CALL DATPUTR (STAT, 'MINUV', MINUV, 1)
      CALL DATPUTR (STAT, 'MAXUV', MAXUV, 1)
      CALL DATPUTR (STAT, 'MINUVW', MINUVW, 1)
      CALL DATPUTR (STAT, 'MAXUVW', MAXUVW, 1)
      CALL DATPUTR (STAT, 'MINUU', MINUU, 1)
      CALL DATPUTR (STAT, 'MAXUU', MAXUU, 1)
      CALL DATPUTR (STAT, 'MINVV', MINVV, 1)
      CALL DATPUTR (STAT, 'MAXVV', MAXVV, 1)
      CALL DATPUTR (STAT, 'MINWW', MINWW, 1)
      CALL DATPUTR (STAT, 'MAXWW', MAXWW, 1)
      CALL DATPUTR (STAT, 'MINTIME', MINTIME, 1)
      CALL DATPUTR (STAT, 'MAXTIME', MAXTIME, 1)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
