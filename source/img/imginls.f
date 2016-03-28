C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imginls.f	1.3 24 Feb 1995
C
      SUBROUTINE IMGINLS (DRT, PSF, XFR, CMP, RES, DWIN, FWIN,
     $   MVIS, INITSLCT, ALG)
C
CD Iteratively NNLS an image
C
C	DRT	CH*(*)	input	Name of Dirty image
C	PSF	CH*(*)	input	Name of Point Spread Function
C	XFR	CH*(*)	input	Name of Transfer Function
C	CMP	CH*(*)	input	Name of CCLEAN image
C	RES	CH*(*)	input	Name of Residual image
C       BOX     CH*(*)  input   Name of Clean Box image
C	INBOX	CH*(*)	input	Name of Input Clean Box image
C	MVIS	CH*(*)	input	Name of FT file
C	INITSLCT CH*(*)	input	Name of initial selection image
C	ALG	CH*(*)	input	Algorithm
C
C In the 'HIST' version of the algorithm, the beam "patch" is determined by
C histogram.  The user selects roughly how many points they would like
C to use, and the routine histograms the residual image, and translates
C it into a flux cutoff.  The points in the patch need not be in any
C particular spatial relationship to each other, but the beam calculated
C will connect them correctly.
C
C The routine iteratively selects the points, creates a data and flux
C subwindow consistent with the windows passed in, convolves the model
C outside the window with the PSF and differences it with the dirty
C image.  The modified dirty image is solved on the subwindows with NNLS.
C A fraction of the new solution on the flux subwindow is accepted as a
C modification of the component model, and the process is iterated.
C
C If INITSLCT is non blank, it will be used as the image for component
C selection on the first iteration and deleted after use.
C
C Audit trail:
C	Original version cloned from imgsvdcl2
C				D.S.Briggs	June 18 1994
C       Eliminate double declaration of variable.
C				M. Stupar	Dec 28 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, PSF, XFR, CMP, RES, DWIN, FWIN, MVIS,
     $   		INITSLCT, ALG
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGINLS')
C
      REAL		GAIN, FLUX, MAXRES, TFLUX
      INTEGER		NITER, NLOC, NPIX(2)
      CHARACTER*1	DATYPE, PATYPE, CATYPE, RATYPE, DWATYPE,
     $   		FWATYPE, ISATYPE
      CHARACTER*(SYSMXNAM)	TEMPFILE, CTIME
      INTEGER		DNAX, DADD, DNAXIS(SYSMXDIM)
      INTEGER		PNAX, PADD, PNAXIS(SYSMXDIM)
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		RNAX, RADD, RNAXIS(SYSMXDIM)
      INTEGER		DWNAX, DWADD, DWNAXIS(SYSMXDIM)
      INTEGER		FWNAX, FWADD, FWNAXIS(SYSMXDIM)
      INTEGER		ISNAX, ISADD, ISNAXIS(SYSMXDIM)
      INTEGER		IAX, NREAL, NDUMMY, NCMP
      INTEGER		IPEAK, PMX, PMY, RNX, RNY, CNX, CNY
      INTEGER		DSADD, FSADD, ITER, NDSWIN, NFSWIN
      REAL		SLIM(2), CURGAIN, S, T
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
      IF (DATEXIST(STRM2(CMP, 'NITER'))) THEN
         CALL DATGETI(CMP, 'NITER', NITER, 1, NDUMMY)
      ELSE
         NITER = 10
         CALL DATPUTI(CMP, 'NITER', NITER, 1)
      END IF
      IF (DATEXIST(STRM2(CMP, 'GAIN'))) THEN
         CALL DATGETR(CMP, 'GAIN', GAIN, 1, NDUMMY)
      ELSE
         GAIN = 0.5
         CALL DATPUTR(CMP, 'GAIN', GAIN, 1)
      END IF
      IF (DATEXIST(STRM2(CMP, 'FLUX'))) THEN
         CALL DATGETR(CMP, 'FLUX', FLUX, 1, NDUMMY)
      ELSE
         FLUX = 0.0
         CALL DATPUTR(CMP, 'FLUX', FLUX, 1)
      END IF
      IF (DATEXIST(STRM2(CMP, 'NPIX'))) THEN
         CALL DATGETI(CMP, 'NPIX', NPIX, 2, NDUMMY)
      ELSE
         NPIX(1) = 1000
         NPIX(2) = 1000
         CALL DATPUTI(CMP, 'NPIX', NPIX, 2)
      END IF
C
C Verify types
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
      CALL DATGETAR (CMP, CNAX, CNAXIS, CATYPE, CADD)
      IF (ERROR) GO TO 990
      IF (CATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//CATYPE//' for Component Image')
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
      CALL DATGETAR (DWIN, DWNAX, DWNAXIS, DWATYPE, DWADD)
      IF (ERROR) GO TO 990
      IF (DWATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//DWATYPE//' for Data Window Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (FWIN, FWNAX, FWNAXIS, FWATYPE, FWADD)
      IF (ERROR) GO TO 990
      IF (FWATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//FWATYPE//' for Flux Window Image')
         GO TO 990
      END IF
C
      IF (INITSLCT.NE.' ') THEN
         CALL DATGETAR (INITSLCT, ISNAX, ISNAXIS, ISATYPE, ISADD)
         IF (ERROR) GO TO 990
         IF (ISATYPE.NE.'R') THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Bad array type '//ISATYPE//' for Initial Select')
            GO TO 990
         END IF
      END IF
C
C Verify sizes
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
         IF (DWNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Data Window and Dirty Axes disagree')
            GO TO 999
         END IF
         IF (FWNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Flux Window and Dirty Axes disagree')
            GO TO 999
         END IF
         IF (INITSLCT.NE.' ') THEN
            IF (ISNAXIS(IAX).NE.DNAXIS(IAX)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1            'Initial Select and Dirty Axes disagree')
               GO TO 999
            END IF
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
C Temp images and arrays we'll need
C
      CALL IMGCLONE (CMP, 'NotComp')
      CALL IMGCLONE (CMP, 'SubComp')
      CALL IMGCLONE (DRT, 'NotDirty')
      CALL IMGCLONE (DRT, 'SubDirty')
      IF (GAIN.LE.0.0) CALL IMGCLONE (DRT, 'DirtyDiff')
      CALL ARRCOPY (RES, 'DataSelect')
      CALL ARRCOPY (RES, 'FluxSelect')
      CALL ARRCOPY (RES, 'FluxNotSelect')
      DSADD = DATADD('DataSelect')
      FSADD = DATADD('FluxSelect')
C
C Finally do something
C
      IF (INITSLCT.EQ.' ') THEN
         CALL IMGRESID (CMP, DRT, XFR, RES, MVIS)
         IF (SYSDEBUG) THEN
            CALL FILIMGPU (RES, 'RES.FULL.0', ' ')
         END IF
      ELSE
         CALL ARRCOPY (INITSLCT, RES)
         CALL DATDELET (INITSLCT)
      END IF
C
C ********************* Start of major cycles ************************
C
      ITER = 1
  1   CONTINUE
C
      WRITE (MESSAGE, 1001) ITER
 1001 FORMAT ('Beginning iteration',I4)
      CALL MSGPUT (' ','I')
      CALL MSGPUT (MESSAGE, 'I')
C
C Select components
C
      CALL ARRCOPY (RES, 'DataSelect')
      CALL PIXRMULT (MEMR(DSADD), MEMR(DWADD), MEMR(DSADD), NLOC)
      CALL ARRCOPY (RES, 'FluxSelect')
      CALL PIXRMULT (MEMR(FSADD), MEMR(FWADD), MEMR(FSADD), NLOC)
C
      IF (NREAL.EQ.2) THEN
C
C Construct histogram and find limiting value such that no more than
C NPIX points will be selected.
C
         CALL ARRABSHI ('DataSelect', 'DataSelect/ABSHIST')
         CALL ARRFLIM ('DataSelect/ABSHIST', NPIX(1), SLIM(1))
         IF (SLIM(1).LT.0.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     1         'Limit in absolute value < 0.0')
            GO TO 999
         END IF
         CALL ARRABSHI ('FluxSelect', 'FluxSelect/ABSHIST')
         CALL ARRFLIM ('FluxSelect/ABSHIST', NPIX(2), SLIM(2))
         IF (SLIM(2).LT.0.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     1         'Limit in absolute value < 0.0')
            GO TO 999
         END IF
C
C Make the masks
C            
         CALL ARRABS ('DataSelect', 'DataSelect')
         CALL ARRCLIP ('DataSelect', SLIM(1), 1.E10, 'DataSelect')
         CALL ARRSCALE ('DataSelect', 1.0, -SLIM(1), 'DataSelect')
         CALL ARRDIV ('DataSelect', 'DataSelect', 'DataSelect')
C
         CALL ARRSTAT ('DataSelect', ' ')
         NDSWIN = DATFGETI ('DataSelect', 'ARRNLOC')
         WRITE (MESSAGE, 1007) NDSWIN
 1007    FORMAT (I5,' pixels in Data subwindow') 
         CALL MSGPUT (MESSAGE,'I')
C
         CALL ARRABS ('FluxSelect', 'FluxSelect')
         CALL ARRCLIP ('FluxSelect', SLIM(2), 1.E10, 'FluxSelect')
         CALL ARRSCALE ('FluxSelect', 1.0, -SLIM(2), 'FluxSelect')
         IF (NITER.GT.1)
     $      CALL ARRMULT ('FluxSelect', 'DataSelect', 'FluxSelect')
         CALL ARRDIV ('FluxSelect', 'FluxSelect', 'FluxSelect')
C
         CALL ARRSTAT ('FluxSelect', ' ')
         NFSWIN = DATFGETI ('FluxSelect', 'ARRNLOC')
         WRITE (MESSAGE, 1008) NFSWIN
 1008    FORMAT (I5,' pixels in Flux subwindow') 
         CALL MSGPUT (MESSAGE,'I')
C
         IF (NFSWIN.LE.1) THEN
            CALL MSGPUT ('Nowhere to put flux -- exiting!','W')
            GO TO 2
         END IF
C
         IF (SYSDEBUG) THEN
            CALL STRNUMFL ('FLUXSEL.#', ITER, TEMPFILE)
            CALL FILIMGPU ('FluxSelect', TEMPFILE, ' ')
            CALL STRNUMFL ('DATASEL.#', ITER, TEMPFILE)
            CALL FILIMGPU ('DataSelect', TEMPFILE, ' ')
         END IF
C
         CALL ARRSETCO ('FluxNotSelect', 0.0, 1.0)
         CALL ARRSUBTR ('FluxNotSelect', 'FluxSelect', 'FluxNotSelect')
C
C Make a Beam matrix
C
         CALL MSGPUT ('Creating Beam matrix','I')
         IF (DATEXIST('BeamMat')) CALL DATDELET ('BeamMat')
         CALL ARRMBMAT ('BeamMat', 'DataSelect', 'FluxSelect', 
     $            PSF, .FALSE.)
C
C Subtract off the sidelobes of the model we're not solving for
C
         CALL MSGPUT ('Subtracting off model sidelobes','I')
         CALL ARRMULT (CMP, 'FluxNotSelect', 'NotComp')
         CALL IMGCONV ('NotComp', ' ', XFR, 'NotDirty', MVIS)
         CALL ARRSUBTR (DRT, 'NotDirty', 'SubDirty')
         IF (SYSDEBUG) THEN
            CALL STRNUMFL ('SUBDRT.#', ITER, TEMPFILE)
            CALL FILIMGPU ('SubDirty', TEMPFILE, ' ')
         END IF
         IF (ERROR) GO TO 999
C            
C Solve for the model in the selected area via NNLS
C
         CALL MSGPUT ('Solving matrix equation with NNLS','I')
         IF (DATEXIST('Data')) CALL DATDELET ('Data')
         IF (DATEXIST('Flux')) CALL DATDELET ('Flux')
         CALL ARRSCOPY ('SubDirty', 'DataSelect', 'Data')
         CALL MATNNLS ('BeamMat', 'Flux', 'Data')
         IF (ERROR) GO TO 999
C
C Determine an appropriate gain
C
         IF (DATEXIST('OldFlux')) CALL DATDELET ('OldFlux')
         CALL ARRSCOPY (CMP, 'FluxSelect', 'OldFlux')
         IF (GAIN.LE.0) THEN
            CALL MSGPUT ('Calculating best residual RMS gain','I')
            IF ((ITER.EQ.1).AND.(INITSLCT.NE.' '))
     $         CALL IMGRESID (CMP, DRT, XFR, RES, MVIS)
            IF (DATEXIST('FluxDiff')) CALL DATDELET ('FluxDiff')
            CALL ARRSUBTR ('Flux', 'OldFlux', 'FluxDiff')
            CALL ARRSETCO ('DirtyDiff', 0.0, 0.0)
            CALL ARRUSCPY ('FluxDiff', 'FluxSelect', 'DirtyDiff')
c           call filimgpu ('DirtyDiff', 'FLUXDIFF', ' ')
            CALL IMGCONV ('DirtyDiff', ' ', XFR, 'DirtyDiff', MVIS)
c           call filimgpu ('DirtyDiff', 'DIRTYDIFF', ' ')
            CALL ARRMULT ('DirtyDiff', DWIN, 'DirtyDiff')
            CALL ARRLEN ('DirtyDiff', T)
            CALL ARRMULT ('DirtyDiff', RES, 'DirtyDiff')
            CALL ARRSUM ('DirtyDiff', S)
            CURGAIN = S / T**2
            WRITE (MESSAGE, 1050) CURGAIN
 1050       FORMAT ('Accepting fraction ',1PE12.4,
     $         ' of current solution')
            CALL MSGPUT (MESSAGE, 'I')
         ELSE
            CURGAIN = GAIN
         END IF
C
C Accept the gain and modify the solution
C
         CALL ARRLC ('OldFlux', 1.0-CURGAIN, 'Flux', CURGAIN, 'Flux')
         CALL ARRMULT (CMP, 'FluxNotSelect', CMP)
         CALL ARRUSCPY ('Flux', 'FluxSelect', CMP)
C
         IF (SYSDEBUG) THEN
            CALL STRNUMFL ('CMP.#', ITER, TEMPFILE)
            CALL FILIMGPU (CMP, TEMPFILE, ' ')
         END IF
C
      ELSE
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'Illegal dimension')
         GO TO 999
      END IF
C
C Now find the updated residual image using an FFT convolution
C
      CALL IMGRESID (CMP, DRT, XFR, RES, MVIS)
      IF (SYSDEBUG) THEN
         CALL STRNUMFL ('RES.#', ITER, TEMPFILE)
         CALL FILIMGPU (RES, TEMPFILE, ' ')
      END IF
      IF (ERROR) GO TO 999
C
C Get statistics from CMP & RES
C
      CALL ARRSTAT (CMP, ' ')
      NCMP = DATFGETI (CMP, 'ARRNLOC')
      TFLUX = DATFGETR (CMP, 'ARRSUM')
      IPEAK = PIXISAMA(RNX*RNY, MEMR(RADD), 1) - 1
      PMX = 1 + MOD(IPEAK, RNX)
      PMY = 1 + (IPEAK - PMX + 1)/RNX
      MAXRES = MEMR(RADD+(PMY-1)*RNX+PMX-1)
C
C Write summary message
C
      CALL MSGPUT (
     1   '  Niter   Nlocations   Residual(Jy/beam)   Total Flux(Jy)',
     2   'I')
C
      WRITE (MESSAGE, 1100) ITER, NCMP, MAXRES, TFLUX
 1100 FORMAT (2X,I6,6X,I7,6X,1PE12.4,7X,1PE12.4)
      CALL MSGPUT (MESSAGE, 'I')
C
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Finished iteration: '//CTIME, 'I')
C
C Go back for more?
C
      IF ((GAIN.LE.0.0).AND.(CURGAIN.LE.ABS(GAIN))) THEN
         CALL MSGPUT ('Solution coverged by gain criterion','I')
         GO TO 2
      END IF
C
      IF (SYSINTRP.AND.(SYSINTAC.EQ.'QUIT')) GO TO 2
      IF ((ABS(MAXRES).GT.FLUX).AND.(ITER.LT.ABS(NITER))) THEN
         ITER = ITER + 1
         GO TO 1
      END IF
C
 2    CONTINUE
C
C ********************* End of major cycles ************************
C
C Store goodies
C
      CALL DATPUTR (CMP, 'TFLUX', TFLUX, 1)
      CALL DATPUTR (CMP, 'FLUX', MAXRES, 1)
      CALL DATPUTI (CMP, 'NITER', ITER, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
