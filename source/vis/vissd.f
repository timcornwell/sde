C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissd.f	1.2    11/7/90
C
      SUBROUTINE VISSD (VIS, CLASS, IMAGE)
C
CD Form single dish image
C
C
C	VIS		CH*(*)	input	Name of visibility data
C	CLASS		CH*(*)	input	Class of visibility data
C	IMAGE		CH*(*)	input	Name of Image
C	DBCHISQ		REAL	output	Chisq be 2nd PB
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, VIS, CLASS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSD')
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
      DOUBLE PRECISION	IRVAL(SYSMXDIM), DTMP
      INTEGER		VNAX, VNAXIS(SYSMXDIM)
      INTEGER 		NDUMMY, RNAX, CRDRNAX
      INTEGER		VSADD, IADD, WTADD, UADD, VADD, DATADD
      CHARACTER*(SYSMXNAM)	SVIS, STRM2
C
      REAL		C(SYSMXDIM), PC(SYSMXDIM)
      DATA	C	/SYSMXDIM*0.0/
      DATA	PC	/SYSMXDIM*0.0/
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IMAGE, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
C
      RNAX = CRDRNAX(INAX, INAXIS)
C
      CALL DATGETD (IMAGE, 'CRVAL', IRVAL, SYSMXDIM, NDUMMY)
      CALL DATGETC (IMAGE, 'CTYPE', ITYPE, SYSMXDIM, NDUMMY)
C
      IF (ERROR) GO TO 990
      CALL DATGETD (IMAGE, 'OBSRA', DTMP, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         PC(1) = IRVAL(1)
         CALL DATPUTD (IMAGE, 'OBSRA', IRVAL(1), 1)
      ELSE
         PC(1) = DTMP
      END IF
      CALL DATGETD (IMAGE, 'OBSDEC', DTMP, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         PC(2) = IRVAL(2)
         CALL DATPUTD (IMAGE, 'OBSDEC', IRVAL(2), 1)
      ELSE
         PC(2) = DTMP
      END IF
      CALL CRDWTOP (IMAGE, PC, C)
C
C Now get UV data
C
      SVIS = STRM2 (VIS, CLASS)
      CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
     1   VROTA)
C
      CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1   VSADD)
      WTADD =  DATADD (STRM2(SVIS, 'WT'))
      IADD = DATADD (IMAGE)
C
      IF (INAXIS(1).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only SD 2 or 3-D')
         GO TO 999
      ELSEIF (INAXIS(2).EQ.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only SD 2 or 3-D')
         GO TO 999
      ELSEIF (INAXIS(3).EQ.1) THEN
C
C ***********************  Two dimensions *************************
C
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         CALL VISSDP (MEMX(VSADD), VNAXIS(1), MEMR(WTADD), MEMR(UADD),
     $      MEMR(VADD), MEMR(IADD), INAXIS(1), INAXIS(2), C(1), C(2))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only SD 2 or 3-D')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
