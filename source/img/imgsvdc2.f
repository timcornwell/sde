C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsvdc2.f	1.3 24 Feb 1995
C
      SUBROUTINE IMGSVDC2 (DRT, PSF, XFR, CCLEAN, RES, BOX, INBOX, MVIS)
C
CD SVD-Clean an image -- version 2  (HIST)
C
C	DRT	CH*(*)	input	Name of Dirty image
C	PSF	CH*(*)	input	Name of Point Spread Function
C	XFR	CH*(*)	input	Name of Transfer Function
C	CCLEAN	CH*(*)	input	Name of CCLEAN image
C	RES	CH*(*)	input	Name of Residual image
C       BOX     CH*(*)  input   Name of Clean Box image
C	INBOX	CH*(*)	input	Name of Input Clean Box image
C	MVIS	CH*(*)	input	Name of FT file
C	ALG	CH*(*)	input	Algorithm
C
C In this version of the algorithm, the beam "patch" is determined by
C histogram.  The user selects roughly how many points they would like
C to use, and the routine histograms the residual image, and translates
C it into a flux cutoff.  The points in the patch need not be in any
C particular spatial relationship to each other, but the beam calculated
C will connect them correctly.
C
C Audit trail:
C	Cloned from imgsvdcl
C				D.S.Briggs	6 Dec 1992
C	Eliminate double declaration of variable.
C				M. Stupar	28 Dec 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, PSF, XFR, CCLEAN, RES, BOX, INBOX, MVIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSVDC2')
C
      REAL		GAIN, FLUX, MAXRES, TFLUX
      INTEGER		NITER, BITER, RITER, ANITER, NLOC
      CHARACTER*1	DATYPE, PATYPE, CATYPE, RATYPE, BATYPE, IBATYPE
      CHARACTER		ATYPE*1
      CHARACTER*(SYSMXNAM)	TEMPFILE
      INTEGER		DNAX, DADD, DNAXIS(SYSMXDIM)
      INTEGER		PNAX, PADD, PNAXIS(SYSMXDIM)
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		RNAX, RADD, RNAXIS(SYSMXDIM)
      INTEGER		BNAX, BADD, BNAXIS(SYSMXDIM)
      INTEGER		IBNAX, IBADD, IBNAXIS(SYSMXDIM)
      INTEGER		NAXIS(SYSMXDIM), IRADD
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD
      INTEGER		RLADD, RXLADD, RYLADD, RZLADD, NPIX(2)
      INTEGER		IAX, NREAL, NDUMMY, NL, NAX, NCMP
      INTEGER		IPEAK, PMX, PMY, RNX, RNY, CNX, CNY
      INTEGER		NIWIN, NSV
      REAL		MINSVR, SLIM(2)
      LOGICAL		DODBLE
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST
      REAL		DATFGETR
      INTEGER		DATFGETI, PIXISAMA, DATADD
C=====================================================================
      IF (ERROR) GO TO 999
C
C Get control parameters
C
      DODBLE = .FALSE.
      MINSVR = 1.E-6
C
      IF (DATEXIST (STRM2 (CCLEAN, 'CCNL'))) THEN
         CALL DATGETI (CCLEAN, 'CCNL', NL, 1, NDUMMY)
      ELSE
         NL = 16384
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'NITER'))) THEN
         CALL DATGETI(CCLEAN, 'NITER', NITER, 1, NDUMMY)
      ELSE
         NITER = 10
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
         GAIN = 0.5
         CALL DATPUTR(CCLEAN, 'GAIN', GAIN, 1)
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'FLUX'))) THEN
         CALL DATGETR(CCLEAN, 'FLUX', FLUX, 1, NDUMMY)
      ELSE
         FLUX = 0.0
         CALL DATPUTR(CCLEAN, 'FLUX', FLUX, 1)
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'NPIX'))) THEN
         CALL DATGETI(CCLEAN, 'NPIX', NPIX, 2, NDUMMY)
         IF (NPIX(2).LE.0) NPIX(2) = NPIX(1)
      ELSE
         NPIX(1) = 500
         NPIX(2) = 500
         CALL DATPUTI(CCLEAN, 'NPIX', NPIX, 2)
      END IF
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
      CNX = CNAXIS(1)
      CNY = CNAXIS(2)
C
      CALL DATGETAR (RES, RNAX, RNAXIS, RATYPE, RADD)
      IF (ERROR) GO TO 990
      IF (RATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//RATYPE//' for Residual Image')
         GO TO 990
      END IF
      RNX = RNAXIS(1)
      RNY = RNAXIS(2)
C
      CALL DATGETAR (BOX, BNAX, BNAXIS, BATYPE, BADD)
      IF (ERROR) GO TO 990
      IF (BATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//BATYPE//' for Clean Box Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (INBOX, IBNAX, IBNAXIS, IBATYPE, IBADD)
      IF (ERROR) GO TO 990
      IF (BATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//IBATYPE//' for Clean Input Box Image')
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
         IF (IBNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Input Clean Box and Dirty Axes disagree')
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
C ********************* Start of major cycles ************************
C
      CALL IMGRESID (CCLEAN, DRT, XFR, RES, MVIS)
      RITER = BITER
  1   CONTINUE
C
         IF (SYSDEBUG) THEN
            CALL STRNUMFL ('RES.FULL.#', RITER, TEMPFILE)
            CALL FILIMGPU (RES, TEMPFILE, ' ')
         END IF
C
C Apply the Clean Boxs to the residual.
C
         CALL PIXRMULT (MEMR(RADD), MEMR(BADD), MEMR(RADD), NLOC)
         IF (INBOX.NE.BOX) THEN
            CALL ARRCOPY (RES, 'INRES')
            IRADD = DATADD('INRES')
            CALL PIXRMULT (MEMR(IRADD), MEMR(IBADD), MEMR(IRADD), NLOC)
         END IF
C
         IF (SYSDEBUG) THEN
            CALL STRNUMFL ('RES.BOX.#', RITER, TEMPFILE)
            CALL FILIMGPU (RES, TEMPFILE, ' ')
         END IF
C
         IF (NREAL.EQ.2) THEN
C
C Construct histogram and find limiting value such that no more than
C NPIX points will be selected.
C
            CALL ARRABSHI (RES, STRM2(RES, 'ABSHIST'))
            CALL ARRFLIM (STRM2(RES, 'ABSHIST'), NPIX(2), SLIM(2))
            IF (SLIM(2).LT.0.0) THEN
               CALL ERRREPOR (ERRLOGIC, ROUTINE,
     1            'Limit in absolute value < 0.0')
               GO TO 999
            END IF
            IF (INBOX.NE.BOX) THEN
               CALL ARRABSHI ('INRES', STRM2('INRES', 'ABSHIST'))
               CALL ARRFLIM (STRM2('INRES', 'ABSHIST'), NPIX(1),
     $            SLIM(1))
               IF (SLIM(1).LT.0.0) THEN
                  CALL ERRREPOR (ERRLOGIC, ROUTINE,
     1               'Limit in absolute value < 0.0')
                  GO TO 999
               END IF
            END IF
C
C Make the masks
C            
            CALL ARRABS (RES, 'BoxedPatch')
            CALL ARRCLIP ('BoxedPatch', SLIM(2), 1.E10, 'BoxedPatch')
            CALL ARRSCALE ('BoxedPatch', 1.0, -SLIM(2), 'BoxedPatch')
            CALL ARRDIV ('BoxedPatch', 'BoxedPatch', 'BoxedPatch')
            CALL ARRSTAT ('BoxedPatch', ' ')
C
            NIWIN = DATFGETI ('BoxedPatch', 'ARRNLOC')
            WRITE (MESSAGE, 1007) NIWIN
 1007       FORMAT (I5,' pixels in beam patch') 
            CALL MSGPUT (MESSAGE,'I')
C
            IF (INBOX.NE.BOX) THEN
               CALL ARRABS (RES, 'InBoxPatch')
               CALL ARRCLIP ('InBoxPatch', SLIM(1), 1.E10, 'InBoxPatch')
               CALL ARRSCALE ('InBoxPatch', 1.0, -SLIM(1), 'InBoxPatch')
               CALL ARRDIV ('InBoxPatch', 'InBoxPatch', 'InBoxPatch')
               CALL ARRSTAT ('InBoxPatch', ' ')
C
               NIWIN = DATFGETI ('InBoxPatch', 'ARRNLOC')
               WRITE (MESSAGE, 1008) NIWIN
 1008          FORMAT (I5,' pixels in input beam patch') 
               CALL MSGPUT (MESSAGE,'I')
            END IF
C
            IF (SYSDEBUG) THEN
               CALL STRNUMFL ('BOX.#', RITER, TEMPFILE)
               CALL FILIMGPU ('BoxedPatch', TEMPFILE, ' ')
            END IF
            IF (ERROR) GO TO 999
C
C Make a Beam matrix and decompose it
C
            CALL MSGPUT ('Creating Beam matrix','I')
            IF (INBOX.EQ.BOX) THEN
               CALL ARRMBMAT ('BeamMat', 'BoxedPatch', 'BoxedPatch',
     $            PSF, .FALSE.)
            ELSE
               CALL ARRMBMAT ('BeamMat', 'InBoxPatch', 'BoxedPatch',
     $            PSF, .FALSE.)
            END IF
C            
            CALL MSGPUT ('Doing Singular Value Decomposition','I')
            CALL MATSVD ('BeamMat', 'U', 'W', 'V')
            CALL DATDELET ('BeamMat')
            IF (ERROR) GO TO 999
C
C Edit the Singular Values
C
            CALL ARRSVEDT ('W', MINSVR, 'W', NSV)
C
C Go get the residual pixels into a linear array
C
            IF (BOX.EQ.INBOX) THEN
               CALL ARRSCOPY (RES, 'BoxedPatch', 'WRes')
            ELSE
               CALL ARRSCOPY (RES, 'InBoxPatch', 'WRes')
            END IF
C
C SVD deconvolve the boxed beam patch
C
            CALL MATSVBKS ('U', 'W', 'V', 'WRes', 'WComp')
C
C Multiply by gain, and add back to component image
C
            CALL ARRSETCO ('WRes', 0.0, 0.0)
            CALL ARRSCALE ('WComp', GAIN, 0.0, 'WComp')
            CALL ARRUSCPY ('WComp', 'BoxedPatch', CCLEAN)
C
            IF (SYSDEBUG) THEN
               CALL STRNUMFL ('CMP.#', RITER, TEMPFILE)
               CALL FILIMGPU (CCLEAN, TEMPFILE, ' ')
            END IF
C
         ELSE
            CALL ERRREPOR(ERRBDARG, ROUTINE, 'Illegal dimension')
            GO TO 999
         END IF
C
C Now find the updated residual image using an FFT convolution
C
         CALL IMGRESID (CCLEAN, DRT, XFR, RES, MVIS)
         IF (ERROR) GO TO 999
C
C Get statistics from CMP
C
         CALL ARRSTAT (CCLEAN, ' ')
         NCMP = DATFGETI (CCLEAN, 'ARRNLOC')
         TFLUX = DATFGETR (CCLEAN, 'ARRSUM')
         IPEAK = PIXISAMA(RNX*RNY, MEMR(RADD), 1) - 1
         PMX = 1 + MOD(IPEAK, RNX)
         PMY = 1 + (IPEAK - PMX + 1)/RNX
         MAXRES = MEMR(RADD+(PMY-1)*RNX+PMX-1)
C
C Write summary message
C
         CALL MSGPUT (
     1      '  Niter   Nlocations   Residual(Jy/beam)   Total Flux(Jy)',
     2      'I')
C
         WRITE (MESSAGE, 1100) RITER, NCMP, MAXRES, TFLUX
 1100    FORMAT (2X,I6,6X,I7,6X,1PE12.4,7X,1PE12.4)
         CALL MSGPUT (MESSAGE, 'I')
         RITER = RITER + 1
C
C delete arrays whose size may change
C
         CALL DATDELET ('WComp')
         CALL DATDELET ('WRes')
         CALL DATDELET ('U')
         CALL DATDELET ('W')
         CALL DATDELET ('V')
C
C Go back for more?
C
      IF (SYSINTRP.AND.SYSINTAC.EQ.'QUIT') GO TO 2
c      IF ((MAXRES.LE.0.0).AND.(FLUX.LE.0.0)) GO TO 2
      IF ((ABS(MAXRES).GT.FLUX).AND.(RITER.LT.ABS(NITER))) GO TO 1
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
