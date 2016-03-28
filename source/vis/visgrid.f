C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visgrid.f	1.2    11/7/90
C
      SUBROUTINE VISGRID (VIS, SUB, IMG, PSF)
C
CD Grid a visibility data set. The output image must exist before 
C calling. The visibility data must be in a standard form. PSF
C determines whether a point spread function is made INSTEAD of
C the usual gridded data. The data will be phase shifted as necessary
C in order to line up the visibility data with the image. This routine
C can handle 1, 2, 3D grids and it can do 3D gridding via a slow 
C transform in Z.
C
C
C	VIS	CH*(*)	input	Name of visibility set
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C	IMG	CH*(*)	input	Name of output image
C	PSF	LOG	input	TRUE for PSF calculation
C Audit trail:
C	Added rotation matrix
C				T.J.Cornwell	Feb 13 1989
C	Now detects slow transform in Z
C				T.J. Cornwell	Feb 18 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, SUB, IMG
      LOGICAL		PSF
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISGRID')
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
      INTEGER		DATADD, VSADD, WTADD, IADD, UADD, VADD,
     1			WADD
      DOUBLE  PRECISION	SMAT (3,3)
      REAL		USCALE, UOFFSET
      REAL		VSCALE, VOFFSET
      REAL		WSCALE, WOFFSET
      REAL		SUMWT
      INTEGER		UORIGIN, VORIGIN, WORIGIN
      CHARACTER*(SYSMXNAM)	SVIS, STRM2, CFTYPE
      LOGICAL		DATEXIST, SLOWZ
C
      INTEGER		CFSUPP, CFOSAMP, CFADD, CFNAX, CFLEN, NDUMMY
      CHARACTER*1	CFATYPE
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
     1      'Cannot find coordinates for input data')
         GO TO 990
      END IF
      CALL CRDGET (IMG, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, IROTA)
      IF (ERROR) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot find coordinates for output image')
         GO TO 990
      END IF
C
C Find rotation matrix required to align data
C
      CALL CRDSHIFT (SVIS, IMG, SMAT)
C
C Set up default for convolution function
C
      IF(.NOT.DATEXIST(STRM2(IMG, 'CFTYPE'))) THEN
         CFTYPE = 'SF'
         CALL DATPUTC (IMG, 'CFTYPE', CFTYPE, 1)
      ELSE
         CALL DATGETC (IMG, 'CFTYPE', CFTYPE, 1, NDUMMY)
      END IF
C
C Make the convolving functions
C
      IF(DATEXIST(STRM2(IMG, 'CFFN'))) THEN
         CALL DATGETAR (STRM2(IMG, 'CFFN'), CFNAX, CFLEN, CFATYPE, 
     1      CFADD)
         CALL GRDCF (CFTYPE, MEMR(CFADD), CFSUPP, CFOSAMP)
         CALL DATGETI (IMG, 'CFOSAMP', CFOSAMP, 1, NDUMMY)
         CALL DATGETI (IMG, 'CFSUPP', CFSUPP, 1, NDUMMY)
      ELSE
         CFNAX = 1
         CFLEN = 1 + 8 * 129
         CFATYPE = 'R'
         CALL DATMAKAR (STRM2(IMG, 'CFFN'), CFNAX, CFLEN, CFATYPE, 
     1      CFADD)
         CALL GRDCF (CFTYPE, MEMR(CFADD), CFSUPP, CFOSAMP)
         CALL DATPUTI (IMG, 'CFOSAMP', CFOSAMP, 1)
         CALL DATPUTI (IMG, 'CFSUPP', CFSUPP, 1)
      END IF
C
C Find addresses of data for the input visibilities, input weights,
C output image, and output weights image
C
      CALL DATGETAR (STRM2(SVIS, 'WT'), VNAX, VNAXIS, VATYPE,
     1   WTADD)
      CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1   VSADD)
      IADD = DATADD (IMG)
C
C Now actually do something: first find number of real axes of the
C output image
C
      INREAL = 0
      DO 5 IAX = 1, INAX
         IF (INAXIS(IAX).GT.1) INREAL = IAX
  5   CONTINUE
      IF (INREAL.EQ.1) THEN
C
C ***********************  One dimension *************************
C
         UADD = DATADD (STRM2(VIS, 'UU'))
         USCALE = 1.0 / IDELT(1)
         UOFFSET = 0.0
         UORIGIN = NINT (IRPIX(1))
         CALL GRDH1D (MEMX(VSADD), MEMR(WTADD), MEMR(UADD), 
     1      VNAXIS(1), USCALE, UOFFSET, UORIGIN, MEMX(IADD), 
     2      PSF, INAXIS(1), MEMR(CFADD), CFSUPP, CFOSAMP, SUMWT)
      ELSEIF (INREAL.EQ.2) THEN
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
         IF (CFTYPE(1:3).EQ.'BOX') THEN
            CALL GRDQH2D (MEMX(VSADD), MEMR(WTADD), MEMR(UADD), 
     1         MEMR(VADD), MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, 
     2         VSCALE, VOFFSET, UORIGIN, VORIGIN, MEMX(IADD), PSF, 
     3         INAXIS(1), INAXIS(2), SMAT, SUMWT)
         ELSE
            CALL GRDH2D (MEMX(VSADD), MEMR(WTADD), MEMR(UADD), 
     1         MEMR(VADD), MEMR(WADD),
     1         VNAXIS(1), USCALE, UOFFSET, VSCALE, VOFFSET, 
     2         UORIGIN, VORIGIN, MEMX(IADD), PSF, INAXIS(1),
     3         INAXIS(2), MEMR(CFADD), CFSUPP, CFOSAMP, MEMR(CFADD), 
     4         CFSUPP, CFOSAMP, SMAT, SUMWT)
         END IF
      ELSEIF (INREAL.EQ.3) THEN
C
C ***********************  Three dimensions *************************
C
         SLOWZ = ITYPE(3).EQ.'N----SIN'
         IF (.NOT.SLOWZ) THEN
            UADD = DATADD (STRM2(VIS, 'UU'))
            VADD = DATADD (STRM2(VIS, 'VV'))
            WADD = DATADD (STRM2(VIS, 'WW'))
            USCALE = 1.0 / IDELT(1)
            UOFFSET = 0.0
            UORIGIN = NINT (IRPIX(1))
            VSCALE = 1.0 / IDELT(2)
            VOFFSET = 0.0
            VORIGIN = NINT (IRPIX(2))
            WSCALE = 1.0 / IDELT(3)
            WOFFSET = 0.0
            WORIGIN = NINT (IRPIX(3))
            IF (CFTYPE(1:3).EQ.'BOX') THEN
               CALL GRDQH3D (MEMX(VSADD), MEMR(WTADD), MEMR(UADD), 
     1            MEMR(VADD), MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, 
     2            VSCALE, VOFFSET, WSCALE, WOFFSET, UORIGIN, VORIGIN, 
     3            WORIGIN, MEMX(IADD), PSF, INAXIS(1),
     4            INAXIS(2), INAXIS(3), SMAT, SUMWT)
            ELSE
               CALL GRDH3D (MEMX(VSADD), MEMR(WTADD), MEMR(UADD), 
     1            MEMR(VADD), MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, 
     2            VSCALE, VOFFSET, WSCALE, WOFFSET, UORIGIN, VORIGIN, 
     3            WORIGIN, MEMX(IADD), PSF, INAXIS(1),
     4            INAXIS(2), INAXIS(3), MEMR(CFADD), CFSUPP, CFOSAMP, 
     5            MEMR(CFADD), CFSUPP, CFOSAMP, MEMR(CFADD), CFSUPP, 
     6            CFOSAMP, SMAT, SUMWT)
            END IF
         ELSE
C
C Slow transform in Z
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
            IF (CFTYPE(1:3).EQ.'BOX') THEN
               CALL GRDQH23D (MEMX(VSADD), MEMR(WTADD), MEMR(UADD), 
     1            MEMR(VADD), MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, 
     2            VSCALE, VOFFSET, UORIGIN, VORIGIN, MEMX(IADD), PSF, 
     3            INAXIS(1), INAXIS(2), INAXIS(3), IDELT(3), IRPIX(3),
     4            SMAT, SUMWT)
            ELSE
               CALL GRDH23D (MEMX(VSADD), MEMR(WTADD), MEMR(UADD), 
     1            MEMR(VADD), MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, 
     2            VSCALE, VOFFSET, UORIGIN, VORIGIN,
     3            MEMX(IADD), PSF, INAXIS(1),
     4            INAXIS(2), MEMR(CFADD), CFSUPP, CFOSAMP, 
     5            MEMR(CFADD), CFSUPP, CFOSAMP, INAXIS(3), IDELT(3),
     6            IRPIX(3), SMAT, SUMWT)
            END IF
         END IF
      ELSE 
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot grid more than 3 dimensions')
         GO TO 999
      END IF
C
C Store sum of weights
C
      CALL DATPUTR (IMG, 'SUMWT', SUMWT, 1)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
