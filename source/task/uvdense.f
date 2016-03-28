C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)uvdense.f	1.4	 1/24/95
C
      SUBROUTINE SDEMAIN
C
CD Program to find UV density
C
C Audit trail:
C	New Program
C				M.A.Holdaway	May 24 1994
C	Changed input cellsize and limits to METERS.
C	Added effective U,V coverage.
C				M.A. Holdaway	May 25, 1994
C	Several new options; also we are doing a MEAN
C	now rather than an RMS as before.
C	Can look at the RMS about the mean.
C				M.A. Holdaway	June 28 1994
C	Also:  grid visibilities too.
C				M.A. Holdaway	Jan 24 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UVDENSE')
C
      REAL              UVCELL(2), MCELL(2)
      INTEGER           UVIMSIZE(2), NBINS
      CHARACTER*(SYSMXNAM)	TELESCOP
      LOGICAL		DOEFFUV, DOVIS, DOWT
      REAL		CELLSIZE(3), PI, TELDIAM, SHAPE(2),
     $			TIME(2), UVLIMITS(2), SHIFT(3), FOV
      REAL		XRANGE(2), YRANGE(2), MLIMITS(2)
      INTEGER 		NDUMMY, DIR, NSEL, TIMR(8), IMSIZE(3)
      LOGICAL		NICEPSF
      CHARACTER*(SYSMXNAM)	VISFILE, WTFILE, CTIME,
     $			SUBCLASS, CFTYPE, DEVMEAN, DEVDISP, TITLCOM,
     $     		FILMEAN, FILDISP, VISAMP, VISPHASE
      INTEGER		NAXV, NAXISV(SYSMXDIM), I
      REAL		RPIXV(SYSMXDIM),ROTAV(SYSMXDIM),DELTV(SYSMXDIM)
      DOUBLE PRECISION	RVALV(SYSMXDIM)
      CHARACTER*8	TYPEV(SYSMXDIM)
      REAL		FREQ, SCALE0, SCALE1, SCALE2, SUMWT, WAVELEN
C
      LOGICAL		DATEXIST
C     
      DATA		SHIFT / 0.0, 0.0, 0.0/
C==================================================================
C
      CALL MSGWELCO ('I find mean uv density')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETR ('Cellsize', MCELL, 2, NDUMMY)
      CALL USRGETI ('Imsize', UVIMSIZE, 2, NDUMMY)
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETL ('NicePSF', NICEPSF, 1, NDUMMY)
      CALL USRGETC ('ConvType', CFTYPE, 1, NDUMMY)
      CALL USRGETR ('Uvlimits', MLIMITS, 2, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETL ('DOWT', DOWT, 1, NDUMMY)
      CALL USRGETL ('DOVIS', DOVIS, 1, NDUMMY)
      CALL USRGETC ('WTGrid', WTFILE, 1, NDUMMY)
      CALL USRGETC ('VISAmp', VISAMP, 1, NDUMMY)
      CALL USRGETC ('VISPhase', VISPHASE, 1, NDUMMY)
C
      CALL USRGETI ('Nbins', NBINS, 1, NDUMMY)
      CALL USRGETR ('Shape', SHAPE, 2, NDUMMY)
      CALL USRGETC ('DevMean', DEVMEAN, 1, NDUMMY)
      CALL USRGETC ('DevDisp', DEVDISP, 1, NDUMMY)
      CALL USRGETC ('FilMean', FILMEAN, 1, NDUMMY)
      CALL USRGETC ('FilDisp', FILDISP, 1, NDUMMY)
      CALL USRGETC ('TitleComment', TITLCOM, 1, NDUMMY)
      CALL USRGETR ('Xrange', XRANGE, 2, NDUMMY)
      CALL USRGETR ('Yrange', YRANGE, 2, NDUMMY)
C
      CALL USRGETL('DoEffectiveUV', DOEFFUV, 1, NDUMMY)
      CALL USRGETC('Telescope', TELESCOP, 1, NDUMMY)
      CALL USRGETR('Teldiam', TELDIAM, 1, NDUMMY)
C
C Now get relevant files
C
      SUBCLASS = 'OBS/I'
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
C
C Convert UV cell size from METERS to wavelengths
C
      CALL CRDGET ('Vis/'//SUBCLASS, NAXV, TYPEV, NAXISV, RVALV, RPIXV,
     $     DELTV, ROTAV)
      FREQ = 0.0
      DO 17 I = 1, NAXV
         IF (TYPEV(I) .EQ. 'FREQ') FREQ = RVALV(I)
 17   CONTINUE
      IF (FREQ .EQ. 0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No Frequency information')
         GOTO 999
      ENDIF
      WAVELEN = 3.0E+8/FREQ
      UVLIMITS(1) = MLIMITS(1)/WAVELEN
      UVLIMITS(2) = MLIMITS(2)/WAVELEN
      UVCELL(1) = MCELL(1)/WAVELEN
      UVCELL(2) = MCELL(2)/WAVELEN
C
C Convert UV plane parameters to Image plane parameters
C
      PI = ATAN2(1.0, 1.0) * 4.0
      IMSIZE(1) = UVIMSIZE(1)
      IMSIZE(2) = UVIMSIZE(2)
      IMSIZE(3) = 1
      CELLSIZE(1) = 1./UVCELL(1) /UVIMSIZE(1) * 3600.*180./PI
      CELLSIZE(2) = 1./UVCELL(2) /UVIMSIZE(2) * 3600.*180./PI
      CELLSIZE(3) = 1.0
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
      CALL IMGMAKE ('Vis/'//SUBCLASS, CELLSIZE, IMSIZE, SHIFT, 
     1   'R', 'PSF')
      IF (ERROR) GO TO 999
      CALL MSGPUT ('Coordinates for PSF:', 'I')
      CALL CRDLIST ('PSF')
C
C Find dual image
C
      NDUMMY = 0
      CALL FFTCONJA ('PSF', 'XFR', DIR, NDUMMY)
C
      CALL MSGPUT ('Coordinates for XFR:', 'I')
      CALL CRDLIST ('XFR')
      IF (ERROR) GO TO 999
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got data: '//CTIME, 'I')
C
C Now grid data and weights
C
      IF (NICEPSF) THEN
         CALL MSGPUT ('Doing NICE PSF weighting of data', 'I')
         CALL IMGMSPSF ('Vis', SUBCLASS, 'XFR', 'PSF')
      ELSE IF (FOV.NE.0.0) THEN
         CALL DATPUTR ('XFR', 'WTFOV', FOV, 1)
         CALL GRDUWT ('Vis', SUBCLASS, 'XFR')
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1100) FOV
 1100    FORMAT ('Weighting for fraction of field of view = ',
     1      F7.2)
         CALL MSGPUT (MESSAGE, 'I')
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Weighting completed: '//CTIME, 'I')
      END IF
C
C Grid Weights
C
      IF (DOWT) THEN
         CALL DATPUTC ('PSF', 'CFTYPE', CFTYPE, 1)
         CALL DATPUTC ('XFR', 'CFTYPE', CFTYPE, 1)
         CALL VISGRID ('Vis', SUBCLASS, 'XFR', .TRUE.)
         IF (DOEFFUV) THEN
            CALL IMGFFT ('XFR', 'PSF')
            CALL DATPUTC ('PSF', 'TELESCOP', TELESCOP, 1)
            CALL DATPUTR ('PSF', 'TELDIAM', TELDIAM, 1)
            CALL IMGPB ('PSF', 'PSF', 'APPLY')
            CALL IMGFFT ('PSF', 'XFR')
         ENDIF
         CALL IMGREAL ('XFR', 'UVcoverage')
         CALL CRDGET ('UVcoverage', NAXV, TYPEV, NAXISV, RVALV, RPIXV,
     $        DELTV, ROTAV)
C     
C     SCALE0 returns the grid to 1 per uv sample
C     
         CALL DATGETR ('UVcoverage', 'SUMWT', SUMWT, 1, NDUMMY)
         SCALE0 = SUMWT/FLOAT((NAXISV(1)-1)*NAXISV(2))
         CALL ARRSCALE ('UVcoverage', SCALE0, 0.0, 'UVcoverage')
         IF (ERROR) GO TO 999
C     
C     SCALE1 sets the scale of the X axis to meters
C     
         FREQ = RVALV(3)
         SCALE1 = ABS(DELTV(1)) * 3.0E+8 / FREQ
C     
         CALL IMGRMEAN ('UVcoverage', 'UVDensity', 
     $        'UVDispersion', 'UVradius', 
     $        NBINS, SCALE1, SHAPE)
         CALL ARRDIV ('UVDispersion', 'UVDensity', 'UVJitter')
C     
C     SCALE2 sets the scale of the Y axis to samples per square meter
C     
         WAVELEN = 3.0E+8/FREQ
         SCALE2 = 1.0/ABS( DELTV(1)*DELTV(2) * WAVELEN**2 )
         CALL ARRSCALE( 'UVDensity', SCALE2, 0.0, 'UVDensity')
         IF (DEVMEAN .NE. ' ') THEN
            CALL ARRPGGRF (NBINS, 'UVradius', 'UVDensity', DEVMEAN, 
     $           'UV radius, m', 'UV point per m2', 
     $           'UV density: '//TITLCOM, 0, 0, XRANGE(1), XRANGE(2),
     $           YRANGE(1), YRANGE(2))
         ENDIF
         IF (DEVDISP .NE. ' ') THEN
            CALL ARRPGGRF (NBINS, 'UVradius', 'UVJitter',
     $           DEVDISP, 'UV radius, m',  'UV_RMS/UV_DENS', 
     $           'UV_RMS/UV_DENS: '//TITLCOM, 0, 0, XRANGE(1), 
     $           XRANGE(2), YRANGE(1), YRANGE(2))
         ENDIF
C     
         CALL DATRENAM ('UVradius', 'PlotData/X')
         CALL DATCREAT ('PlotData')
         IF (FILMEAN .NE. ' ') THEN
            CALL DATPUTC ('PlotData', 'COMMENT', 
     $           'Mean UV Density: '//TITLCOM, 1)
            CALL DATPUTC ('PlotData', 'LABEL', ' ', 1)
            CALL DATPUTL ('PlotData', 'DISCRETE', .FALSE., 1)
            CALL DATRENAM ('UVDensity', 'PlotData/Y')
            CALL FILPUTPF ('PlotData', FILMEAN)
            CALL DATRENAM ('PlotData/Y', 'UVDensity')
         ENDIF
         IF (FILDISP .NE. ' ') THEN
            CALL DATPUTC ('PlotData', 'COMMENT', 
     $           'RMS of UV Density: '//TITLCOM, 1)
            CALL DATPUTC ('PlotData', 'LABEL', ' ', 1)
            CALL DATPUTL ('PlotData', 'DISCRETE', .FALSE., 1)
            CALL DATRENAM ('UVDispersion', 'PlotData/Y')
            CALL FILPUTPF ('PlotData', FILDISP)
            CALL DATRENAM ('PlotData/Y', 'UVDispersion')
         ENDIF
C     
C     Write Gridded UV Coverage
C     
         IF (WTFILE .NE. ' ') THEN
            CALL FILIMGPU ('UVcoverage', WTFILE, ' ')
         ENDIF
      ENDIF
C
C Grid Visibilities
C
      IF (DOVIS) THEN
         CALL DATPUTC ('PSF', 'CFTYPE', CFTYPE, 1)
         CALL DATPUTC ('XFR', 'CFTYPE', CFTYPE, 1)
         CALL VISGRID ('Vis', SUBCLASS, 'XFR', .FALSE.)
         CALL ARRX2AP ('XFR', 'AMP', 'PHASE')
         IF (DATEXIST ('UVcoverage')) THEN
            CALL ARRDIV ('AMP', 'UVcoverage', 'AMP')
            CALL MSGPUT ('We have divided the gridded vis by the'//
     $           ' gridded weights', 'I')
         ELSE
            CALL MSGPUT ('We have NOT divided the gridded vis by the'//
     $           ' gridded weights', 'W')
         ENDIF
C
C     Write Gridded UV Visibilities
C     
         IF (VISAMP .NE. ' ') THEN
            CALL FILIMGPU ('AMP', VISAMP, ' ')
         ENDIF
         IF (VISPHASE .NE. ' ') THEN
            CALL FILIMGPU ('PHASE', VISPHASE, ' ')
         ENDIF
      ENDIF


C     
 999     CONTINUE
         END
