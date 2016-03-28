C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)sidgrid.f	1.2	 7/20/92
C
      SUBROUTINE SIDGRID (SID, SUB, IMG)
C
C Grid a single dishdata set. The output image must exist before 
C calling. 
C
C	SID	CH*(*)	input	Name of visibility set
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C	IMG	CH*(*)	input	Name of output image
C Audit trail:
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	SID, SUB, IMG
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SIDGRID')
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
      INTEGER		DATADD, VSADD, WTADD, IADD, RAADD, DECADD
      REAL		RASCALE, RAOFFSET
      REAL		DCSCALE, DCOFFSET
      REAL		SUMWT
      INTEGER		RAORIGIN, DCORIGIN
      CHARACTER*(SYSMXNAM)	SSID, STRM2, CFTYPE
      LOGICAL		DATEXIST
C
      INTEGER		CFSUPP, CFOSAMP, CFADD, CFNAX, CFLEN, NDUMMY
      CHARACTER*1	CFATYPE
C==========================================================================
      IF (ERROR) GO TO 999
C
C Find coordinate information
C
      SSID = STRM2 (SID, SUB)
      CALL CRDGET (SSID, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
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
      CALL DATGETAR (STRM2(SSID, 'WT'), VNAX, VNAXIS, VATYPE,
     1   WTADD)
      CALL DATGETAR (STRM2(SSID, 'SID'), VNAX, VNAXIS, VATYPE,
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
         RAADD = DATADD (STRM2(SID, 'RA'))
         RASCALE = 1.0 / IDELT(1)
         RAOFFSET = 0.0
         RAORIGIN = NINT (IRPIX(1))
         CALL GRDR1D (MEMR(VSADD), MEMR(WTADD), MEMR(RAADD), 
     1      VNAXIS(1), RASCALE, RAOFFSET, RAORIGIN, MEMR(IADD), 
     2      INAXIS(1), MEMR(CFADD), CFSUPP, CFOSAMP, SUMWT)
      ELSEIF (INREAL.EQ.2) THEN
C
C ***********************  Two dimensions *************************
C
         RAADD = DATADD (STRM2(SID, 'RA'))
         DECADD = DATADD (STRM2(SID, 'DEC'))
         RASCALE = 1.0 / IDELT(1)
         RAOFFSET = 0.0
         RAORIGIN = NINT (IRPIX(1))
         DCSCALE = 1.0 / IDELT(2)
         DCOFFSET = 0.0
         DCORIGIN = NINT (IRPIX(2))
         IF (CFTYPE(1:3).EQ.'BOX') THEN
            CALL GRDQR2D (MEMR(VSADD), MEMR(WTADD), MEMR(RAADD), 
     1         MEMR(DECADD), VNAXIS(1), RASCALE, RAOFFSET, 
     2         DCSCALE, DCOFFSET, RAORIGIN, DCORIGIN, MEMR(IADD), 
     3         INAXIS(1), INAXIS(2), SUMWT)
         ELSE
            CALL GRD2D (MEMR(VSADD), MEMR(WTADD), MEMR(RAADD), 
     1         MEMR(DECADD), 
     1         VNAXIS(1), RASCALE, RAOFFSET, DCSCALE, DCOFFSET, 
     2         RAORIGIN, DCORIGIN, MEMR(IADD), INAXIS(1),
     3         INAXIS(2), MEMR(CFADD), CFSUPP, CFOSAMP, MEMR(CFADD), 
     4         CFSUPP, CFOSAMP, SUMWT)
         END IF
      ELSE 
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot grid more than 2 dimensions')
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
