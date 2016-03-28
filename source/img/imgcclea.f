C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgcclea.f	1.7    7/27/92
C
      SUBROUTINE IMGCCLEA (DRT, PSF, XFR, CCLEAN, RES, BOX, MVIS)
C
CD Clark-Clean an image.
C
C	DRT	CH*(*)	input	Name of Dirty image
C	PSF	CH*(*)	input	Name of Point Spread Function
C	XFR	CH*(*)	input	Name of Transfer Function
C	CCLEAN	CH*(*)	input	Name of CCLEAN image
C	RES	CH*(*)	input	Name of Residual image
C       BOX     CH*(*)  input   Name of Clean Box image
C	MVIS	CH*(*)	input	Name of FT file
C Audit trail:
C	Now outputs correct number of components
C				T.J.Cornwell	Feb 9 1989
C	Now calculates addresses of clean components correctly
C	on second call even if number of iterations has changed.
C				T.J.Cornwell	Feb 14 1989
C	Changed output format to be a bit more compact
C				T.J.Cornwell	Feb 15 1989
C	Stop at first neg if FLUX < 0.0
C				Your name	Date
C       Added Generalised Clean Box
C                               R.G. Marson     Dec 20 1990
C       Fixed bug in calculation of NLOC
C				D.S.Briggs	June 2 1992
C	Added fields
C				T.J. Cornwell	July 16 1992
C	Added extra argument in PIX2DRCC, etc.
C				T.J. Cornwell	July 27 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, PSF, XFR, CCLEAN, RES, BOX, MVIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCCLEA')
C
      REAL		GAIN, FLUX, MAXRES, TFLUX, SLIM, SPEED
      INTEGER		NITER, BITER, RITER, ANITER, NLOC
      CHARACTER*1	DATYPE, PATYPE, CATYPE, RATYPE, BATYPE
      INTEGER		DNAX, DADD, DNAXIS(SYSMXDIM)
      INTEGER		PNAX, PADD, PNAXIS(SYSMXDIM)
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		RNAX, RADD, RNAXIS(SYSMXDIM)
      INTEGER		BNAX, BADD, BNAXIS(SYSMXDIM)
      INTEGER		NAXIS(SYSMXDIM)
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD, CFLADD, FN
      INTEGER		RLADD, RXLADD, RYLADD, RZLADD
      INTEGER		IAX, NREAL, NDUMMY, NL, ANL, NAX
      REAL		AMAX, AMIN, EXTSIDE
      LOGICAL		DATEXIST
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get control parameters
C
      IF (DATEXIST(STRM2(CCLEAN, 'FIELD'))) THEN
         CALL DATGETI(CCLEAN, 'FIELD', FN, 1, NDUMMY)
      ELSE
         FN = 0
      END IF
      IF (DATEXIST (STRM2 (PSF, 'EXTSIDE'))) THEN
         CALL DATGETR (PSF, 'EXTSIDE', EXTSIDE, 1, NDUMMY)
      ELSE
         EXTSIDE = 0.1
      END IF
      IF (DATEXIST (STRM2 (CCLEAN, 'CCNL'))) THEN
         CALL DATGETI (CCLEAN, 'CCNL', NL, 1, NDUMMY)
      ELSE
         NL = 16384
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'NITER'))) THEN
         CALL DATGETI(CCLEAN, 'NITER', NITER, 1, NDUMMY)
      ELSE
         NITER = 100
         CALL DATPUTI(CCLEAN, 'NITER', NITER, 1)
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'BITER'))) THEN
         CALL DATGETI(CCLEAN, 'BITER', BITER, 1, NDUMMY)
      ELSE
         BITER = 1
         CALL DATPUTI(CCLEAN, 'BITER', BITER, 1)
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'GAIN'))) THEN
         CALL DATGETR(CCLEAN, 'GAIN', GAIN, 1, NDUMMY)
      ELSE
         GAIN = 0.1
         CALL DATPUTR(CCLEAN, 'GAIN', GAIN, 1)
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'FLUX'))) THEN
         CALL DATGETR(CCLEAN, 'FLUX', FLUX, 1, NDUMMY)
      ELSE
         FLUX = 0.0
         CALL DATPUTR(CCLEAN, 'FLUX', FLUX, 1)
      END IF
      SPEED = 1.0
C
C Make lists
C
      CALL DATGETAR (DRT, DNAX, DNAXIS, DATYPE, DADD)
      IF (ERROR) GO TO 990
      IF (DATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//DATYPE//' for Dirty Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (PSF, PNAX, PNAXIS, PATYPE, PADD)
      IF (ERROR) GO TO 990
      IF (PATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//PATYPE//' for PSF')
         GO TO 990
      END IF
C
      CALL DATGETAR (CCLEAN, CNAX, CNAXIS, CATYPE, CADD)
      IF (ERROR) GO TO 990
      IF (CATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//CATYPE//' for CLEAN Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (RES, RNAX, RNAXIS, RATYPE, RADD)
      IF (ERROR) GO TO 990
      IF (RATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//RATYPE//' for Residual Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (BOX, BNAX, BNAXIS, BATYPE, BADD)
      IF (ERROR) GO TO 990
      IF (BATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//BATYPE//' for Clean Box Image')
         GO TO 990
      END IF
C
      NREAL = 0
      NLOC = 1
      DO 10 IAX = 1, SYSMXDIM
         IF (CNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Clean and Dirty Axes disagree')
            GO TO 990
         END IF
         IF (RNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Residual and Dirty Axes disagree')
            GO TO 999
         END IF
         IF (BNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Clean Box and Dirty Axes disagree')
            GO TO 999
         END IF
         IF (DNAXIS(IAX).GT.1) THEN
            NREAL = NREAL + 1
            NLOC = NLOC * DNAXIS(IAX)
         ELSE
            GO TO 11
         END IF
  10  CONTINUE
  11  CONTINUE
C
C Make locations for components list
C
      IF (.NOT.DATEXIST (STRM2(CCLEAN, 'PIXLIST'))) THEN
         NAX = 1
         NAXIS(1) = ABS(NITER)
         ATYPE = 'R'
         CALL DATMAKAR (STRM2(CCLEAN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         ATYPE='I'
         CALL DATMAKAR (STRM2(CCLEAN, 'PIXFIELD'), NAX, NAXIS, ATYPE,
     1     CFLADD)
         NAX = 2
         ATYPE = 'I'
         NAXIS(2) = NREAL
         CALL DATMAKAR (STRM2(CCLEAN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2 * NAXIS(1)
      ELSE
         CALL DATGETAR (STRM2(CCLEAN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         CALL DATGETAR (STRM2(CCLEAN, 'PIXFIELD'), NAX, NAXIS, ATYPE,
     1     CFLADD)
         CALL DATGETAR (STRM2(CCLEAN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2 * NAXIS(1)
      END IF
C
C Now make locations for residual list
C
      IF (.NOT.DATEXIST (STRM2(RES, 'PIXLIST'))) THEN
         NAX = 1
         NAXIS(1) = NL
         ATYPE = 'R'
         CALL DATMAKAR (STRM2(RES, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     RLADD)
         NAX = 2
         ATYPE = 'I'
         NAXIS(2) = NREAL
         CALL DATMAKAR (STRM2(RES, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     RXLADD)
         RYLADD = RXLADD + NAXIS(1)
         RZLADD = RXLADD + 2 * NAXIS(1)
      ELSE
         CALL DATGETAR (STRM2(RES, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     RLADD)
         CALL DATGETAR (STRM2(RES, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     RXLADD)
         RYLADD = RXLADD + NAXIS(1)
         RZLADD = RXLADD + 2 * NAXIS(1)
      END IF
C
C Finally do something
C
      CALL MSGPUT (
     1   '  Nlocations     Ncomps   Residual(Jy/beam)   Total Flux(Jy)',
     2   'I')
C
C ********************* Start of major cycles ************************
C
      CALL IMGRESID (CCLEAN, DRT, XFR, RES, MVIS)
      RITER = BITER
C
C Apply the Clean Box to the residual. The Clean Box is an image which is
C 1.0 at locations where cleaning can occur and 0.0 at other locations. This
C can be genralised to 'soft boxes' by introducing intermediate values. So 
C that a large enough residual outside the clean box will eventually have 
C to be cleaned.
C
  1   CONTINUE
         CALL PIXRMULT (MEMR(RADD), MEMR(BADD), MEMR(RADD), NLOC)
         CALL ARRSTAT (RES, ' ')
         CALL DATGETR (RES, 'ARRMAX', AMAX, 1, NDUMMY)
         CALL DATGETR (RES, 'ARRMIN', AMIN, 1, NDUMMY)
         MAXRES = MAX(ABS(AMAX), ABS(AMIN))
C
C Construct histogram and find limiting value such that no more than
C NL points will be selected. Then ensure that this limit is no
C lower than the peak residual times the highest exterior sidelobe
C
         CALL ARRABSHI (RES, STRM2(RES, 'ABSHIST'))
         CALL ARRFLIM (STRM2(RES, 'ABSHIST'), NL, SLIM)
         IF (SLIM.LT.0.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     1         'Limit in absolute value < 0.0')
            GO TO 999
         END IF
         SLIM = MAX(SLIM, ABS(EXTSIDE * MAXRES))
         IF (NREAL.EQ.2) THEN
C
C Construct the list of pixels having values > SLIM. ANL is the actual
C number of such values.
C
            CALL PIX2DRLI (MEMR(RADD), RNAXIS(1), RNAXIS(2), SLIM, 
     1         MEMR(RLADD), MEMI(RXLADD), MEMI(RYLADD), NL, ANL)
            IF (ERROR) GO TO 999
C
C Now do minor cycle clean on the list of residuals stopping at the greater
C of the specified flux limit or the cutoff in residuals. We must pass
C both FLUX and SLIM since the latter must be used to limit the
C number of clean components found.
C
            CALL PIX2DRCC (MEMR(RLADD), MEMI(RXLADD), MEMI(RYLADD),
     1         ANL, MEMR(PADD), PNAXIS(1), PNAXIS(2), GAIN, NITER, 
     2         RITER, RITER, FLUX, SPEED, SLIM, MEMR(CLADD), 
     3         MEMI(CXLADD), MEMI(CYLADD), MEMI(CFLADD), FN, ANITER,
     $         MAXRES, TFLUX)
C
C Convert the list of clean components into an image (we actually
C add the specified clean components to the current image)
C
            CALL PIX2DRMA (MEMR(CLADD), MEMI(CFLADD), FN,
     $         MEMI(CXLADD), MEMI(CYLADD), 
     1         RITER, ANITER, MEMR(CADD), CNAXIS(1), CNAXIS(2))
         ELSE IF (NREAL.EQ.3) THEN
            CALL PIX3DRLI (MEMR(RADD), RNAXIS(1), RNAXIS(2), 
     1         RNAXIS(3), SLIM, MEMR(RLADD), MEMI(RXLADD), 
     2         MEMI(RYLADD), MEMI(RZLADD), NL, ANL)
            CALL PIX3DRCC (MEMR(RLADD), MEMI(RXLADD), MEMI(RYLADD),
     1         MEMI(RZLADD), ANL, MEMR(PADD), PNAXIS(1), PNAXIS(2), 
     2         PNAXIS(3), GAIN, NITER, RITER, RITER, FLUX, SPEED, SLIM, 
     3         MEMR(CLADD), MEMI(CXLADD), MEMI(CYLADD), MEMI(CZLADD), 
     4         MEMI(CFLADD), FN, ANITER, MAXRES, TFLUX)
            CALL PIX3DRMA (MEMR(CLADD), MEMI(CFLADD), FN,
     $         MEMI(CXLADD), MEMI(CYLADD), 
     1         MEMI(CZLADD), RITER, ANITER, MEMR(CADD), CNAXIS(1), 
     2         CNAXIS(2), CNAXIS(3))
         ELSE
            CALL ERRREPOR( ERRBDARG, ROUTINE, 'Illegal dimension')
            GO TO 999
         END IF
         RITER = ANITER + 1
C
C Now find the updated residual image using an FFT convolution
C
         CALL IMGRESID (CCLEAN, DRT, XFR, RES, MVIS)
         IF (ERROR) GO TO 999
C
C Write summary message
C
         WRITE (MESSAGE, 1000) ANL, ANITER, MAXRES, TFLUX
 1000    FORMAT (2X,I6,6X,I7,6X,1PE12.4,7X,1PE12.4)
         CALL MSGPUT (MESSAGE, 'I')
C
C Go back for more?
C
      IF (SYSINTRP.AND.SYSINTAC.EQ.'QUIT') GO TO 2
      IF ((MAXRES.LE.0.0).AND.(FLUX.LE.0.0)) GO TO 2
      IF ((ABS(MAXRES).GT.FLUX).AND.(ANITER.LT.ABS(NITER))) GO TO 1
 2    CONTINUE
C
C ********************* End of major cycles ************************
C
C Store goodies
C
      CALL DATPUTR (CCLEAN, 'TFLUX', TFLUX, 1)
      CALL DATPUTR (CCLEAN, 'FLUX', MAXRES, 1)
      CALL DATPUTI (CCLEAN, 'NITER', ANITER, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
