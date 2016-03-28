C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)uvmap.f	1.16	 1/18/96
C
      SUBROUTINE SDEMAIN
C
CD Program to map visibility data
C
C Audit trail:
C	Gave in and put in normalization
C				T.J.Cornwell	Feb 4 1989
C	Changed to VISTOIMG and IMGTOVIS
C				T.J. Cornwell	Feb 18 1989
C	Skip selection by VISSEL if no TIME axis
C				T.J. Cornwell	June 20 1990
C	Added DFT option
C				M.A.Holdaway	June 23 1991
C	Added NICEPSF option	
C				M.A.Holdaway	July 1 1991
C	Added FFTSIZE option
C				D.S.Briggs	Sept 28 1992
C	Added inputs history to dirty map
C				D.S.Briggs	May 19 1993
C	Added proper calculation of expected thermal noise
C				D.S.Briggs	July 22 1993
C	Added robust uniform weighting, beam fitting, more noise stats,
C	ungridded/robust uniform weighting.  VLBI weighting.
C				D.S.Briggs	Aug 24 1993
C	Added the FullBin option, and redid the estimated running time
C	routines.
C				D.S.Briggs	Jan 25 1994
C	Added WeightImage
C				D.S.Briggs	Oct 10 1994
C	Added azimuthal robustness and tacked inputs onto WeightImage.
C	Added some histogram and weight trimming code as comments,
C	for the rare occasions when they are needed.
C				D.S.Briggs	Oct 23 1994
C	Added VisOut for exporting exotic weightings to other packages.
C				D.S.Briggs	Nov 1 1994
C	Convolution type wasn't getting passed properly when only DRT is
C	calculated.  Reorganised output of images, so that PSF can be
C	reused for DRT calculations.  Very ugly, but it makes a
C	signficant difference for huge images.  Warn user that scale is
C	off if only DRT calculated.
C				D.S.Briggs	Nov 28 1994
C	Changed VLBI weighting to take an arbitrary exponent instead
C	of just square root.
C				D.S.Briggs	Jan 16 1995
C	Put thermal noise stats in HISTORY comments
C				D.S.Briggs	Feb 19 1995
C	Added weight histogram switch to inputs, enabling the historgram
C	code that was formerly only in comments
C				D.S.Briggs	March 25 1995
C	Now reads in a TEMPLATE image to place grid upon, ignoring
C	IMSIZE, CELLSIZE, SHIFT
C				M.A. Holdaway	Jan 15 1996
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UVMAP')
C
      REAL		CELLSIZE(3), TAPER(3), MAXPSF, SUMWT, TSUMWT,
     $			TIME(2), UVLIMITS(2), SHIFT(3), FOV, DS(2),
     $   		THRMS, ROBUST(3), NARMS, RMSRATIO, VLBPOW
      INTEGER 		NDUMMY, DIR, NSEL, TIMR(8), IMSIZE(3)
      LOGICAL		SLOWZ, DFT, NICEPSF
      CHARACTER*(SYSMXNAM)	VISFILE, DRTFILE, PSFFILE, CTIME,
     $			STOKES, SUBCLASS, CFTYPE, FFTSIZE, SVWTS, RDWTS,
     $			RMODE, WTIMG, VISOUT, TEMPLATE
      CHARACTER*80	RMSMSG, THERMMSG
      LOGICAL		DOFIT, DOTAPER, DOGRID, DOVLBI, DOWTHIST
C
      REAL		WTMIN, WTMAX, WTAVE, WTSUM, WTDISP
      INTEGER		WTGRDSIZ(2), BINDEBUG
C
      INTEGER		ARRNPIX, ARRNPOS, DATADD
      LOGICAL		DATEXIST
      REAL		DATFGETR
      CHARACTER*(SYSMXNAM)	STRM3
C==================================================================
C
      CALL MSGWELCO ('I make images from visibility data')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETC ('Template', TEMPLATE, 1, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 3, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETL ('GridWeights', DOGRID, 1, NDUMMY)
      CALL USRGETI ('GridSize', WTGRDSIZ, 2, NDUMMY)
      CALL USRGETI ('BinDebug', BINDEBUG, 1, NDUMMY)
      CALL USRGETC ('SaveWeights', SVWTS, 1, NDUMMY)
      CALL USRGETC ('ReadWeights', RDWTS, 1, NDUMMY)
      CALL USRGETC ('WeightImage', WTIMG, 1, NDUMMY)
      CALL USRGETL ('WtHist', DOWTHIST, 1, NDUMMY)
      CALL USRGETR ('DS', DS, 2, NDUMMY)
      CALL USRGETC ('RMode', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, RMODE)
      CALL USRGETR ('Robust', ROBUST, 3, NDUMMY)
      CALL USRGETR ('VLBpower', VLBPOW, 1, NDUMMY)
      CALL USRGETL ('NicePSF', NICEPSF, 1, NDUMMY)
      CALL USRGETL ('FitPSF', DOFIT, 1, NDUMMY)
      CALL USRGETC ('ConvType', CFTYPE, 1, NDUMMY)
      CALL USRGETC ('FFTSize', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, FFTSIZE)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('Filter', TAPER, 3, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETL ('SlowZ', SLOWZ, 1, NDUMMY)
      CALL USRGETL ('DFT', DFT, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('VisOut', VISOUT, 1, NDUMMY)
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
      SLOWZ = SLOWZ .AND. (IMSIZE(3).GT.1)
      DOTAPER = (TAPER(1).NE.0.0).OR.(TAPER(2).NE.0.0)
      DOVLBI = VLBPOW .NE. 1.0
      RMSMSG = ' '
      THERMMSG = ' '
C
C Enforce some restrictions
C
      IF (FFTSIZE.EQ.' ') FFTSIZE = 'PWR2'
      IF ((FFTSIZE.NE.'PWR2').AND.(FFTSIZE.NE.'EXACT')) THEN
         CALL MSGPUT ('FFTSIZE forced to PWR2','I')
         FFTSIZE = 'PWR2'
      END IF
C
      IF (RMODE.EQ.' ') RMODE = 'NONE'
      IF ((RMODE.NE.'NORM').AND.(RMODE.NE.'ABS').AND.
     $    (RMODE.NE.'NONE').AND.(RMODE.NE.'AZ-NORM')) THEN
         CALL MSGPUT ('RMODE forced to NONE','I')
         RMODE = 'NONE'
      END IF
C
      IF ((RMODE.EQ.'ABS').AND.(DS(1).LE.0.0)) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE,
     $      'Absolute robust weighting needs a flux scale -- use DS!')
         GO TO 999
      END IF
C
C Subclass to be gridded
C
      SUBCLASS = 'OBS/'//STOKES(1:1)
C
C Now get relevant files
C
      CALL VISGET ('Vis', VISFILE, STOKES(1:1), '*', ' ')
C
      IF(DATEXIST('Vis/TIME')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL ('Vis', SUBCLASS, TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No TIME information', 'W')
      END IF
C
C Make output image
C
      IF (TEMPLATE .EQ. ' ') THEN
         CALL IMGMAKE ('Vis/'//SUBCLASS, CELLSIZE, IMSIZE, SHIFT, 
     1        'R', 'PSF')
      ELSE
         CALL FILIMGGE ('TEMPLATE', TEMPLATE, '*')
         CALL IMGDOUBL ('TEMPLATE', 'PSF')
         CALL DATDELET ('TEMPLATE')
      ENDIF
      CALL DATPUTC ('PSF', 'FFTSIZE', FFTSIZE, 1)
      IF (ERROR) GO TO 999
      CALL MSGPUT ('Coordinates for PSF:', 'I')
      CALL CRDLIST ('PSF')
C
C Find dual image
C
      IF (SLOWZ) THEN
         CALL MSGPUT ('Fourier sum in w', 'I')
         NDUMMY = 2
         CALL FFTCONJA ('PSF', 'XFR', DIR, NDUMMY)
      ELSE
         NDUMMY = 0
         CALL FFTCONJA ('PSF', 'XFR', DIR, NDUMMY)
      END IF
      CALL MSGPUT ('Coordinates for XFR:', 'I')
      CALL CRDLIST ('XFR')
      IF (ERROR) GO TO 999
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got data: '//CTIME, 'I')
C
C Thermal noise calculation setup.
C   We are assume that incoming weights are statistical
C
      IF ((DS(1).GT.0.0).AND.(DS(2).GT.0.0))
     $   DS(1) = DS(1) * SQRT(DS(2))
      CALL ARRCOPY (STRM3('Vis', SUBCLASS, 'WT'), 'OrigWeights')
C
C Calculate sensitivity of an untapered naturally weighted image for
C  reference, if we won't do it later.
C
      IF ((FOV.GT.0.0).OR.DOTAPER.OR.DOVLBI) THEN
         CALL ARRSCOPY (STRM3('Vis', SUBCLASS, 'WT'),
     $      'OrigWeights', 'PosWeights')
         CALL ARRSUM ('PosWeights', SUMWT)
         NARMS = SQRT(1.0 / SUMWT)
         CALL DATDELET ('PosWeights')
         IF (DS(1).GT.0.0) NARMS = NARMS * DS(1)
      END IF
C
C VLBI weighting?
C
      IF (DOVLBI) THEN
 1010    FORMAT ('Raising all weights to the power ',F7.3)
         WRITE (MESSAGE, 1010) VLBPOW
         CALL MSGPUT (MESSAGE,'I')
         CALL ARRPOWER (STRM3('Vis', SUBCLASS, 'WT'), VLBPOW, -99.0,
     $                 STRM3('Vis', SUBCLASS, 'WT'))
      END IF
C
C Now grid data and weights
C
      IF (NICEPSF) THEN
         CALL MSGPUT ('Doing NICE PSF weighting of data', 'I')
         CALL IMGMSPSF ('Vis', SUBCLASS, 'XFR', 'PSF')
      ELSE IF (FOV.NE.0.0) THEN
         WRITE (MESSAGE, 1030) FOV
 1030    FORMAT ('Weighting for fraction of field of view =', F7.2)
         CALL MSGPUT (MESSAGE, 'I')
         IF (DOFIT) THEN
            CALL ARRSCOPY (STRM3('Vis', SUBCLASS, 'WT'),
     $         'OrigWeights', 'PosWeights')
            CALL ARRSTAT('PosWeights', ' ')
            IF (ERROR) GO TO 999
C
            WTMIN = DATFGETR('PosWeights', 'ARRMIN')
            WTMAX = DATFGETR('PosWeights', 'ARRMAX')
            WTAVE = DATFGETR('PosWeights', 'ARRAVE')
            WTSUM = DATFGETR('PosWeights', 'ARRSUM')
            WTDISP = DATFGETR('PosWeights', 'ARRDISP')
            CALL DATDELET ('PosWeights')
C
            CALL MSGPUT ('Weights:  min, max, avg, sum, disp','I')
            WRITE (MESSAGE, 1050) WTMIN, WTMAX, WTAVE, WTSUM, WTDISP
 1050       FORMAT (3F10.4,F12.2,F10.4)
            CALL MSGPUT (MESSAGE,'I')
         END IF
         CALL DATPUTR ('XFR', 'WTFOV', FOV, 1)
         CALL DATPUTC ('XFR', 'RMODE', RMODE, 1)
         IF (RMODE.NE.'NONE') THEN
            IF (RMODE.EQ.'NORM') THEN
               WRITE (MESSAGE, 1100) ROBUST(1)
 1100          FORMAT ('Normalized robust parameter =',F10.4)
            ELSE IF (RMODE.EQ.'AZ-NORM') THEN
               WRITE (MESSAGE, 1102) ROBUST
 1102          FORMAT ('Azimuthal normalized robust parameters =',
     $            2F10.4,F9.3)
            ELSE IF (RMODE.EQ.'ABS') THEN
               WRITE (MESSAGE, 1105) ROBUST(1)
 1105          FORMAT ('Robust reference flux =',F9.4)
            END IF
            CALL MSGPUT (MESSAGE, 'I')
            CALL DATPUTR ('XFR', 'ROBUST', ROBUST, 3)
            IF (RMODE.EQ.'ABS') CALL DATPUTR ('XFR', 'DS', DS, 1)
         END IF
         IF (DOGRID) THEN
            IF (RMODE.NE.'NONE') THEN
               CALL GRDRUWT ('Vis', SUBCLASS, 'XFR')
            ELSE
               CALL GRDUWT ('Vis', SUBCLASS, 'XFR')
            END IF
         ELSE
            CALL MSGPUT ('Doing slow weighting -- be patient','I')
            CALL DATPUTI ('XFR', 'WTGRDSIZ', WTGRDSIZ, 2)
            CALL DATPUTI ('XFR', 'BINDEBUG', BINDEBUG, 1)
            IF (SVWTS.NE.' ')
     $         CALL DATPUTC ('XFR', 'SAVEWEIGHTS', SVWTS, 1)
            IF (RDWTS.NE.' ')
     $         CALL DATPUTC ('XFR', 'READWEIGHTS', RDWTS, 1)
            CALL GRDURUWT ('Vis', SUBCLASS, 'XFR')
         END IF
         IF (ERROR) GO TO 999
         IF (DOFIT) THEN
            CALL ARRSCOPY (STRM3('Vis', SUBCLASS, 'WT'),
     $         'OrigWeights', 'PosWeights')
            CALL ARRSTAT('PosWeights', ' ')
            IF (ERROR) GO TO 999
C
            WTMIN = DATFGETR('PosWeights', 'ARRMIN')
            WTMAX = DATFGETR('PosWeights', 'ARRMAX')
            WTAVE = DATFGETR('PosWeights', 'ARRAVE')
            WTSUM = DATFGETR('PosWeights', 'ARRSUM')
            WTDISP = DATFGETR('PosWeights', 'ARRDISP')
            CALL DATDELET ('PosWeights')
C
            CALL MSGPUT (
     $         'Adjusted weights:  min, max, avg, sum, disp','I')
            WRITE (MESSAGE, 1110) WTMIN, WTMAX, WTAVE, WTSUM, WTDISP
 1110       FORMAT (3F10.4,F12.2,F10.4)
            CALL MSGPUT (MESSAGE,'I')
         END IF
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Weighting completed: '//CTIME, 'I')
      END IF
C
C Apply taper
C
      IF (DOTAPER) THEN
         CALL VISTAPER ('Vis', SUBCLASS, TAPER, SUBCLASS)
         IF (ERROR) GO TO 999
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Filtering: '//CTIME, 'I')
      END IF
      IF (ERROR) GO TO 999
C
C Some histograms
C
      IF (DOWTHIST) THEN
         CALL MSGPUT ('Writing weight histogram to WTHIST.ps','I')
         CALL ARRSCOPY (STRM3('Vis', SUBCLASS, 'WT'),
     $      'OrigWeights', 'PosWeights')
         CALL ARRSCOPY ('OrigWeights', 'OrigWeights', 'OPosWeights')
         CALL PIXPGHIS (ARRNPIX('PosWeights'),
     $      MEMR(DATADD('PosWeights')), 0.0, 0.0, 100, 'WTHIST.ps/ps',
     $      'Weights', 'Visibility Weights')
         CALL FILARRPU ('PosWeights','POSWEIGHTS')
C
         CALL MSGPUT ('Writing amplification histogram to AMPLHIST.ps',
     $      'I')
         CALL ARRDIV ('PosWeights','OPosWeights','Ampl')
         CALL PIXPGHIS (ARRNPIX('Ampl'), MEMR(DATADD('Ampl')), 0.0,
     $      0.0, 100, 'AMPLHIST.ps/ps', 'Amplification',
     $      'Visibility Weight Amplification')
      END IF
C
C Trim weights by amplification threshold, normally commented out.
C
c      CALL MSGPUT ('Trimming weights by amplification threshold','I')
c      CALL ARRCLIP2 ('Ampl', STRM3('Vis', SUBCLASS, 'WT'), 0.0, -1.0,
c     $   .075, -1.0, STRM3('Vis', SUBCLASS, 'WT'))
c      CALL ARRFLAG (STRM3('Vis', SUBCLASS, 'WT'), 'Ampl',
c     $   STRM3('Vis', SUBCLASS, 'WT'))
c      WRITE (MESSAGE, 1130) ARRNPOS(STRM3('Vis', SUBCLASS, 'WT'))
c 1130 FORMAT (I7, ' positive weights after trimming')
c      CALL MSGPUT (MESSAGE,'I')
C
C Back to normal code here
C
      IF (DATEXIST('PosWeights')) CALL DATDELET ('PosWeights')
      IF (DATEXIST('OPosWeights')) CALL DATDELET ('OPosWeights')
      IF (DATEXIST('Ampl')) CALL DATDELET ('Ampl')
C
C Calculate expected thermal noise.  (See Felli & Spencer p. 177)
C
      CALL ARRSCOPY (STRM3('Vis', SUBCLASS, 'WT'),
     $   STRM3('Vis', SUBCLASS, 'WT'), 'PosWeights')
      CALL ARRSCOPY ('OrigWeights',
     $   STRM3('Vis', SUBCLASS, 'WT'), 'OPosWeights')
      CALL ARRMULT ('PosWeights', 'PosWeights', 'TmpWeights')
      CALL ARRDIV ('TmpWeights', 'OPosWeights', 'TmpWeights')
      CALL ARRSUM ('TmpWeights', TSUMWT)
      CALL ARRSUM ('PosWeights', SUMWT)
      THRMS = SQRT(TSUMWT) / SUMWT
      IF (DS(1).GT.0.0) THEN
         THRMS = THRMS * DS(1)
         WRITE (THERMMSG, 1150) THRMS
 1150    FORMAT ('Expected RMS thermal noise is',1PG11.3,' JY/BEAM')
      ELSE
         WRITE (THERMMSG, 1160) THRMS
 1160    FORMAT ('Expected RMS thermal noise is Delta S (unit wt) times'
     $      ,1PG11.3)
      END IF
      CALL MSGPUT (THERMMSG, 'I')
      IF ((FOV.GT.0.0).OR.DOTAPER.OR.DOVLBI) THEN
         RMSRATIO = THRMS / NARMS
         WRITE (RMSMSG, 1170) RMSRATIO
 1170    FORMAT ('Thermal RMS degraded by factor of ',F7.3,
     $      ' from best case')
         CALL MSGPUT (RMSMSG, 'I')
      END IF
C
      CALL DATDELET ('PosWeights')
      CALL DATDELET ('OPosWeights')
      CALL DATDELET ('TmpWeights')
      IF (ERROR) GO TO 999
C
C Weight Image
C
      IF (WTIMG.NE.' ') THEN
         IF (CFTYPE.NE.'BOX')
     $      CALL MSGPUT ('ConvType = BOX might be more useful','W')
         CALL MSGPUT ('Gridding for Weight Image','I')
         CALL IMGCLONE ('XFR', 'WeightImage')
         CALL DATPUTC ('WeightImage', 'CFTYPE', CFTYPE, 1)
         CALL DATPUTL ('WeightImage', 'SLOWZ', SLOWZ, 1)
         CALL VISGRID ('Vis', SUBCLASS, 'WeightImage', .TRUE.)
         CALL ARRCVTR ('WeightImage','WeightImage')
         CALL HISINPUT ('WeightImage')
         CALL FILIMGPU ('WeightImage', WTIMG, ' ')
      END IF
C
C DFT or FFT?
C
      MAXPSF = 0.0
C
      IF (DFT) THEN
         CALL IMGCLONE ('PSF', 'Dirty')
         CALL VISDFT   ('Vis', SUBCLASS, 'Dirty', 'PSF')
         IF (ERROR) GO TO 999
      ELSE
C
C Only make PSF if required
C
         IF (DOFIT.OR.(PSFFILE.NE.' ')) THEN
            CALL MSGPUT ('Calculating PSF','I')
            CALL DATPUTC ('PSF', 'CFTYPE', CFTYPE, 1)
            CALL DATPUTC ('XFR', 'CFTYPE', CFTYPE, 1)
            CALL VISTOIMG ('Vis', SUBCLASS, 'PSF', 'XFR', .TRUE.)
            CALL DATGETR ('XFR', 'SUMWT', SUMWT, 1, NDUMMY)
            CALL DATPUTR ('PSF', 'SUMWT', SUMWT, 1)
            WRITE (MESSAGE,1200) SUMWT
 1200       FORMAT ('Sum of weights = ',1PE15.6)
            CALL MSGPUT (MESSAGE, 'I')
            CALL ARRSTAT ('PSF', ' ')
            CALL DATGETR ('PSF', 'ARRMAX', MAXPSF, 1, NDUMMY)
            IF (MAXPSF.EQ.0.0) THEN
               CALL ERRREPOR (ERRNTFND, ROUTINE, 'Peak of PSF zero')
               GO TO 999
            ELSE
               CALL ARRSCALE('PSF',1.0/MAXPSF,0.0,'PSF')
            END IF
C
C If we're interested in beam statistics, we may as well do all of them
C
            IF (DOFIT) THEN
               CALL MSGPUT ('Old Style AIPS/SDE Beam Fit:','I')
               CALL DATPUTC ('PSF','FIT-ALG','ARCHIVE',1)
               CALL IMGBMSHP( 'PSF' )
               CALL MSGPUT ('Linear Beam Fit:','I')
               CALL DATPUTC ('PSF','FIT-ALG','LINEAR',1)
               CALL IMGBMSHP( 'PSF' )
               CALL MSGPUT ('Non-Linear Beam Fit:','I')
               CALL DATPUTC ('PSF','FIT-ALG','NONLINEAR',1)
               CALL IMGBMSHP( 'PSF' )
            END IF
            CALL SYSETIME (CTIME)
            CALL MSGPUT ('Made PSF: '//CTIME, 'I')
C
C Write PSF if required
C
            IF (PSFFILE.NE.' ') THEN         
               CALL HISOPEN ('PSF')
               CALL HISINPUT ('PSF')
               CALL HISPUT ('PSF', RMSMSG)
               CALL FILIMGPU ('PSF', PSFFILE, ' ')
            ENDIF
C
         END IF
         IF (ERROR) GO TO 999
C
C Only do Dirty if required
C
         IF (DRTFILE.NE.' ') THEN
            CALL MSGPUT ('Calculating Dirty Map','I')
            CALL DATRENAM ('PSF', 'Dirty')
            CALL DATRENAM ('XFR', 'GriddedVis')
            CALL DATPUTC ('GriddedVis', 'CFTYPE', CFTYPE, 1)
            CALL VISTOIMG ('Vis', SUBCLASS, 'Dirty', 'GriddedVis', 
     $         .FALSE.)
            CALL ARRSTAT ('Dirty', ' ')
            IF (MAXPSF.NE.0.0) THEN
               CALL ARRSCALE('Dirty',1.0/MAXPSF,0.0,'Dirty')
            ELSE
               CALL MSGPUT (
     $            'WARNING!: Scale of dirty beam will be slightly off!',
     $            'W')
            END IF
            CALL SYSETIME (CTIME)
            IF (ERROR) GO TO 999
            CALL MSGPUT ('Made Dirty: '//CTIME, 'I')
         END IF
      ENDIF
C
C Write out VIS and DRT if requested
C
      IF (VISOUT.NE.' ') THEN
         CALL HISOPEN ('Vis')
         CALL HISINPUT ('Vis')
         CALL HISPUT ('Vis', RMSMSG)
         CALL HISPUT ('Vis', THERMMSG)
         CALL VISPUT ('Vis', VISOUT, 'OBS', STOKES(1:1), '*', ' ')
      END IF
      IF (DRTFILE.NE.' ') THEN
         IF (PSFFILE.EQ.' ') THEN
            CALL HISOPEN ('Dirty')
            CALL HISINPUT ('Dirty')
            CALL HISPUT ('Dirty', RMSMSG)
         END IF
         CALL HISPUT ('Dirty', THERMMSG)
         CALL FILIMGPU ('Dirty', DRTFILE, ' ')
      ENDIF
C
 999  CONTINUE
      END
