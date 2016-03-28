C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdegri.f	1.2    11/7/90
C
      SUBROUTINE VISDEGRI (VIS, SUB, IMG)
C
CD Degrid a visibility data set. The output dataset must exist before 
C calling. The visibility data must be in a standard form.
C
C
C	VIS	CH*(*)	input	Name of visibility set
C	SUB	CH*(*)	input	Name of sub-class to degrid e.g. OBS/I
C	IMG	CH*(*)	input	Name of image
C Audit trail:
C	Changed to rotation matrix
C				T.J.Cornwell	Feb 13 1989
C      Changed to Double for SHIFT
C				T.J.Cornwell	Jan 3 1990
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, SUB, IMG
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDEGRI')
C
      CHARACTER*1	VATYPE
      CHARACTER*8	VTYPE(SYSMXDIM)
      REAL		VRPIX(SYSMXDIM), VDELT(SYSMXDIM),
     1			VROTA(SYSMXDIM)
      DOUBLE PRECISION	VRVAL(SYSMXDIM)
      INTEGER		VNAX, VNAXIS(SYSMXDIM)
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      INTEGER		INAX, IAX, INAXIS(SYSMXDIM), INREAL
      INTEGER		DATADD, VSADD, WTADD, IADD, UADD, VADD, WADD
      REAL		USCALE, UOFFSET
      REAL		VSCALE, VOFFSET
      INTEGER		UORIGIN, VORIGIN
      CHARACTER*(SYSMXNAM)	SVIS, STRM2, IFTYPE
      LOGICAL		DATEXIST, SLOWZ
C
      INTEGER		IFSUPP, IFOSAMP, IFADD, IFNAX, IFLEN, NDUMMY
      CHARACTER*1	IFATYPE
      DOUBLE PRECISION	SHIFT(3,3)
C==========================================================================
      IF (ERROR) GO TO 999
C
C Find coordinate information
C
      SVIS = STRM2 (VIS, SUB)
      CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
     1   VROTA)
      IF (ERROR) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot find coordinates for output data')
         GO TO 990
      END IF
      CALL CRDGET (IMG, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, IROTA)
      IF (ERROR) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot find coordinates for input image')
         GO TO 990
      END IF
C
C Find shift required to align data
C
      CALL CRDSHIFT (SVIS, IMG, SHIFT)
C
C Set up default for convolution function
C
      IF(.NOT.DATEXIST(STRM2(IMG, 'IFTYPE'))) THEN
         IFTYPE = 'SF'
         CALL DATPUTC (IMG, 'IFTYPE', IFTYPE, 1)
      ELSE
         CALL DATGETC (IMG, 'IFTYPE', IFTYPE, 1, NDUMMY)
      END IF
C
C Make the convolving functions
C
      IF(DATEXIST(STRM2(IMG, 'IFFN'))) THEN
         CALL DATGETAR (STRM2(IMG, 'IFFN'), IFNAX, IFLEN, IFATYPE, 
     1      IFADD)
         CALL GRDCF (IFTYPE, MEMR(IFADD), IFSUPP, IFOSAMP)
      ELSE
         IFNAX = 1
         IFLEN = 1 + 8 * 128
         IFATYPE = 'R'
         CALL DATMAKAR (STRM2(IMG, 'IFFN'), IFNAX, IFLEN, IFATYPE, 
     1      IFADD)
         CALL GRDCF (IFTYPE, MEMR(IFADD), IFSUPP, IFOSAMP)
         CALL DATPUTI (IMG, 'IFOSAMP', IFOSAMP, 1)
         CALL DATPUTI (IMG, 'IFSUPP', IFSUPP, 1)
      END IF
C
C Find addresses of data for the output visibilities, output weights,
C input image
C
      CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1   VSADD)
      WTADD =  DATADD (STRM2(SVIS, 'WT'))
      IADD = DATADD (IMG)
C
C Now actually do something: first find number of real axes of the
C input image
C
      INREAL = 0
      DO 5 IAX = 1, INAX
         IF (INAXIS(IAX).GT.1) INREAL = IAX
  5   CONTINUE
C
C Slow transform in Z
C
      SLOWZ = ITYPE(3).EQ.'N----SIN'
C
      IF (INREAL.EQ.2) THEN
C
C ***********************  Two dimensions *************************
C
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         WADD = DATADD (STRM2(VIS, 'WW'))
         USCALE = 1.0 / IDELT(1)
         UOFFSET = 0.0
         UORIGIN = NINT (IRPIX(1))
         VSCALE = 1.0 / IDELT(2)
         VOFFSET = 0.0
         VORIGIN = NINT (IRPIX(2))
         CALL GRDDEH2D (MEMX(VSADD), MEMR(WTADD), MEMR(UADD), 
     1      MEMR(VADD), MEMR(WADD),
     1      VNAXIS(1), USCALE, UOFFSET, VSCALE, VOFFSET, 
     2      UORIGIN, VORIGIN, MEMX(IADD), INAXIS(1),
     3      INAXIS(2), MEMR(IFADD), IFSUPP, IFOSAMP, MEMR(IFADD), 
     4      IFSUPP, IFOSAMP, SHIFT)
      ELSE IF ((INREAL.EQ.3).AND.(SLOWZ)) THEN
C
C ***********************  Three dimensions *************************
C
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         WADD = DATADD (STRM2(VIS, 'WW'))
         USCALE = 1.0 / IDELT(1)
         UOFFSET = 0.0
         UORIGIN = NINT (IRPIX(1))
         VSCALE = 1.0 / IDELT(2)
         VOFFSET = 0.0
         VORIGIN = NINT (IRPIX(2))
         CALL GRDDEH23 (MEMX(VSADD), MEMR(WTADD), MEMR(UADD), 
     1      MEMR(VADD), MEMR(WADD),
     1      VNAXIS(1), USCALE, UOFFSET, VSCALE, VOFFSET, 
     2      UORIGIN, VORIGIN, MEMX(IADD), INAXIS(1),
     3      INAXIS(2), MEMR(IFADD), IFSUPP, IFOSAMP, MEMR(IFADD), 
     4      IFSUPP, IFOSAMP, INAXIS(3), IDELT(3),
     6      IRPIX(3), SHIFT)
      ELSE 
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot degrid more than 3 dimensions')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
