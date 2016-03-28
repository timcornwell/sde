C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdft.f	1.2    11/7/90
C
      SUBROUTINE VISDFT (VIS, CLASS, IMAGE, PSF)
C
CD Direct Fourier transform to Image from Vis. data
C
C
C	VIS		CH*(*)	input	Name of visibility data
C	CLASS		CH*(*)	input	Class of visibility data
C	IMAGE		CH*(*)	input	Name of Image
C	PSF		CH*(*)	input	Name of PSF
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	This only needs one routine to call: VISDFT3D
C				T.J. Cornwell	Oct 26 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, PSF, VIS, CLASS
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDFT')
C
      CHARACTER*1	VATYPE
      CHARACTER*8	VTYPE(SYSMXDIM)
      REAL		VRPIX(SYSMXDIM), VDELT(SYSMXDIM),
     1			VROTA(SYSMXDIM)
      DOUBLE PRECISION	VRVAL(SYSMXDIM)
      INTEGER		INAX, INAXIS(SYSMXDIM)
      CHARACTER*1	IATYPE
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      INTEGER		VNAX, VNAXIS(SYSMXDIM)
      INTEGER 		NDUMMY, IAX
      INTEGER		VSADD, IADD, PADD, WTADD, UADD, VADD, DATADD,
     1			WADD
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
      PADD = DATADD (PSF)
C
      IF (INAXIS(1).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFT 2 or 3-D')
         GO TO 999
      ELSEIF (INAXIS(2).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFT 2 or 3-D')
         GO TO 999
      ELSEIF ((INAXIS(3).EQ.1).OR.(INAXIS(4).EQ.1)) THEN
C
C ******************** Two or Three dimensions ******************
C
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         WADD = DATADD (STRM2(VIS, 'WW'))
         CALL VISDFT3D (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), 
     1      MEMR(UADD), MEMR(VADD), MEMR(WADD), MEMR(IADD), 
     2      MEMR(PADD), INAXIS(1), INAXIS(2), INAXIS(3), 
     3      IRPIX(1), IDELT(1), IRPIX(2), IDELT(2), IRPIX(3), IDELT(3))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only DFT 2 or 3-D')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
