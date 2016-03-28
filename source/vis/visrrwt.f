C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrrwt.f	1.2	 6/28/94
C
      SUBROUTINE VISRRWT (VIS, SUB, GRADIUS, GDENSE, SHAPE)
C
CD Radial reweighting of a visibility set 
C
C
C	VIS	CH*(*)	input	Name of visibility set
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C	GRADIUS	CH*(*)	input	Name of r array
C	GDENSE	CH*(*)	input	Name of D(r) array
C	SHAPE	R(2)	input	S(1) = Elongation, S(2) = PA of major axis
C	
C Audit trail:
C	New
C				M.A. Holdaway	June 9, 1994
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, SUB, GRADIUS, GDENSE
      REAL		SHAPE(2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRRWT')
C
      CHARACTER*1	VATYPE
      INTEGER		VNAX, VNAXIS(SYSMXDIM), GDNAX,GDNAXIS(SYSMXDIM)
      INTEGER		DATADD, WTADD, UADD, VADD,
     1			GRADD, GDADD      
      CHARACTER*(SYSMXNAM)	SVIS, STRM2
      CHARACTER*1	GDATYPE
C==========================================================================
      IF (ERROR) GO TO 999
C
      SVIS = STRM2 (VIS, SUB)
C
      CALL DATGETAR (STRM2(SVIS, 'WT'), VNAX, VNAXIS, VATYPE,
     1     WTADD)
      UADD = DATADD (STRM2(VIS, 'UU'))
      VADD = DATADD (STRM2(VIS, 'VV'))
      CALL DATGETAR (GRADIUS, GDNAX, GDNAXIS, GDATYPE,
     1     GRADD)
      GDADD = DATADD (GDENSE)
C
      CALL VISRRWTP ( MEMR(WTADD), MEMR(UADD), 
     $     MEMR(VADD), VNAXIS(1), MEMR(GRADD),
     $     MEMR(GDADD), GDNAXIS(1), SHAPE)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
