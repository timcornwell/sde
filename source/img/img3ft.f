C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)img3ft.f	1.3 9/18/92
C
      SUBROUTINE IMG3FT (VIS, CLASS, IMAGE)
C
CD Direct Fourier transform from Image to Vis. data 
C This is 3-dimensionally correct.
C
C
C	VIS		CH*(*)	input	Name of visibility data
C	CLASS		CH*(*)	input	Class of visibility data
C	IMAGE		CH*(*)	input	Name of Image
C Audit trail:
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, VIS, CLASS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMG3FT')
C
      CHARACTER*1	VATYPE
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
      DOUBLE PRECISION	SMAT(3,3)
C
      INTEGER		VSADD, IADD, WTADD, UADD, VADD, WADD, DATADD
      CHARACTER*(SYSMXNAM)	SVIS, STRM2
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      REAL		C(SYSMXDIM), PC(SYSMXDIM), CBT(SYSMXDIM)
      REAL		GAIN1, GAIN2
      DATA	C	/SYSMXDIM*0.0/
      DATA	CBT	/SYSMXDIM*0.0/
      DATA	PC	/SYSMXDIM*0.0/
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IMAGE, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
C
C Get Images
C
      IADD = DATADD (IMAGE)
      IF (INAXIS(1).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only 3FT 2 or 3-D')
         GO TO 999
      ELSEIF (INAXIS(2).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only 3FT 2 or 3-D')
         GO TO 999
      ELSEIF (INAXIS(3).EQ.1) THEN
C
C Get UV data
C
         SVIS = STRM2 (VIS, CLASS)
         CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, 
     1   VDELT, VROTA)
C
C Find rotation matrix required to align data
C
         CALL CRDSHIFT (SVIS, IMAGE, SMAT)
C
         CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1      VSADD)
         WTADD =  DATADD (STRM2(SVIS, 'WT'))
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         WADD = DATADD (STRM2(VIS, 'WW'))
C
         CALL IMG3FT2 (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     1         MEMR(UADD), MEMR(VADD), MEMR(WADD), MEMR(IADD), 
     2         INAXIS(1), INAXIS(2), IRPIX(1), IDELT(1), IRPIX(2), 
     3         IDELT(2), SMAT)
      ELSEIF (INAXIS(4).EQ.1) THEN
C
C Get UV data
C
         SVIS = STRM2 (VIS, CLASS)
         CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, 
     1   VDELT, VROTA)
C
C Find rotation matrix required to align data
C
         CALL CRDSHIFT (SVIS, IMAGE, SMAT)
C
         CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1      VSADD)
         WTADD =  DATADD (STRM2(SVIS, 'WT'))
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         WADD = DATADD (STRM2(VIS, 'WW'))
C
         CALL IMG3FT3 (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     1         MEMR(UADD), MEMR(VADD), MEMR(WADD), MEMR(IADD), 
     2         INAXIS(1), INAXIS(2), INAXIS(3), IRPIX(1), IDELT(1), 
     3         IRPIX(2), IDELT(2), IRPIX(3), IDELT(3), SMAT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only 3FT 2 or 3-D')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
