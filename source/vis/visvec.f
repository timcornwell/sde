C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visvec.f	1.2 11/18/94
C
      SUBROUTINE VISVEC (VIS, CLASS, VECTOR)
C
CD Convert visibilities to Vector
C
C	VIS		CH*(*)	input	Name of visibility data
C	CLASS		CH*(*)	input	Class of visibility data
C	VECTOR		CH*(*)	input	Name of Data Vector
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Nov 16 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VECTOR, VIS, CLASS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISVEC')
C
      CHARACTER*1	VATYPE, MATYPE
      INTEGER		VNAXIS(SYSMXDIM), MNAXIS(SYSMXDIM), VNAX,
     $   		MNAX
C
      LOGICAL		DATEXIST
      INTEGER		VSADD, WTADD, DATADD, VECADD
      CHARACTER*(SYSMXNAM)	SVIS, STRM2
C==================================================================
      IF (ERROR) GO TO 999
C
      SVIS = STRM2 (VIS, CLASS)
C
      CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1   VSADD)
      WTADD =  DATADD (STRM2(SVIS, 'WT'))
C
C Create Vector
C
      IF(.NOT.DATEXIST(VECTOR)) THEN
         MNAXIS(1) = 2 * VNAXIS(1)
         CALL DATMAKAR (VECTOR, 1, MNAXIS, 'R', VECADD)
      ELSE
         CALL DATGETAR (VECTOR, MNAX, MNAXIS, MATYPE, VECADD)
      ENDIF
C
      CALL VISVECP (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     $   MEMR(VECADD), MNAXIS(1))
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
