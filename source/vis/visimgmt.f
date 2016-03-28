C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visimgmt.f	1.2 11/18/94
C
      SUBROUTINE VISIMGMT (VIS, CLASS, IMAGE, WINDOW, IVECTOR,
     $   MATRIX)
C
CD Create Matrix for Image->VIS transform
C
C	VIS		CH*(*)	input	Name of visibility data
C	CLASS		CH*(*)	input	Class of visibility data
C	IMAGE		CH*(*)	input	Name of Image
C	WINDOW		CH*(*)	input	Name of Window Image
C	MAT		CH*(*)	input	Name of Matrix
C	IVECTOR		CH*(*)	input	Name of Image Vector
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Nov 16 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, WINDOW, MATRIX,  VIS,
     $   IVECTOR, CLASS
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISIMGMT')
C
      LOGICAL		DATEXIST
      CHARACTER*1	VATYPE, WATYPE, IVATYPE, MATYPE
      CHARACTER*8	VTYPE(SYSMXDIM)
      REAL		VRPIX(SYSMXDIM), VDELT(SYSMXDIM),
     1			VROTA(SYSMXDIM)
      DOUBLE PRECISION	VRVAL(SYSMXDIM)
      INTEGER		INAX, INAXIS(SYSMXDIM)
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      INTEGER		VNAX, VNAXIS(SYSMXDIM)
      INTEGER		MNAX, MNAXIS(SYSMXDIM)
      INTEGER		WNAX, WNAXIS(SYSMXDIM)
      INTEGER		IVNAX, IVNAXIS(SYSMXDIM)
      INTEGER 		NDUMMY
      INTEGER		VSADD, IADD, WINADD, WTADD, UADD, VADD, 
     1			DATADD, WADD, MATADD, VECADD, NPIXEL
      CHARACTER*(SYSMXNAM)	SVIS, STRM2
C==================================================================
      IF (ERROR) GO TO 999
C
      SVIS = STRM2 (VIS, CLASS)
      CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
     1   VROTA)
      CALL CRDGET (IMAGE, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
C
      CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1   VSADD)
      WTADD =  DATADD (STRM2(SVIS, 'WT'))
      IADD = DATADD (IMAGE)
      CALL DATGETAR (WINDOW, WNAX, WNAXIS, WATYPE, WINADD)
      CALL DATGETAR (IVECTOR, IVNAX, IVNAXIS, IVATYPE, NDUMMY)
C
C Get Matrix and Vector
C
      CALL DATGETAR (MATRIX, MNAX, MNAXIS, MATYPE, MATADD)
C
      IF (INAXIS(1).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only process 2 or 3-D')
         GO TO 999
      ELSEIF (INAXIS(2).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only process 2 or 3-D')
         GO TO 999
      ELSEIF ((INAXIS(3).EQ.1).OR.(INAXIS(4).EQ.1)) THEN
C
C ******************** Two or Three dimensions ******************
C
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         WADD = DATADD (STRM2(VIS, 'WW'))
         CALL VISIMGMP (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     1      MEMR(UADD), MEMR(VADD), MEMR(WADD), MEMR(IADD),
     2      MEMR(WINADD), INAXIS(1), INAXIS(2), INAXIS(3), 
     $      MEMR(MATADD), MNAXIS(1), MNAXIS(2),
     3      IRPIX(1), IDELT(1), IRPIX(2), IDELT(2), IRPIX(3), IDELT(3))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Can only process 2 or 3-D')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
