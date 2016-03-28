C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)cohtst.f	1.4    6/13/92
C
      SUBROUTINE SDEMAIN
C
CD Program to perform a test for coherence
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added null CLEAN box to track changes to IMGCLEAN
C				D.S.Briggs	June 10 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'COHTST')
C
      REAL		CELLSIZE(3), IMSIZE(3), SCALE, OFFSET,
     1			TIME(2), UVLIMITS(2), TAVG,
     2			TBEG, TEND
      INTEGER 		NDUMMY, I, DIR, NSEL, NIMAGE, TIMR(8)
      CHARACTER*(SYSMXNAM)	VISFILE
      INTEGER		NITER, ANITER,
     2			BLC (SYSMXDIM), TRC (SYSMXDIM),
     3			STEP (SYSMXDIM)
      REAL		FLUX, TFLUX, GAIN, MAXRES, BFACT, BEAM(4),
     1			ZFLUX, FACT
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), 
     1			ROTA(SYSMXDIM), SHIFT(3)
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	CLNFILE, RESFILE, DRTFILE, PSFFILE,
     1			SCLNFILE, CTIME, NUMBFILE, GLFILE, PLFILE
      CHARACTER*6	LIMAGE, STRINT
      CHARACTER*12	STRTIMC
C
      DATA		STEP	/SYSMXDIM*1/
      DATA		SHIFT	/3*0.0/
C==================================================================
C
      CALL MSGWELCO ('I perform a coherence test')
      CALL USRCTL
C
C ************************************************************
C
C                         Mapping section
C
C ************************************************************
C
C Get information from user
C
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETR ('Taverage', TAVG, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETR ('ZFlux', ZFLUX, 1, NDUMMY)
      CALL USRGETC ('Outfile', GLFILE, 1, NDUMMY)
      CALL USRGETC ('Plotfile', PLFILE, 1, NDUMMY)
C
C Now get relevant files
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
C
C Do initial selection
C
      CALL UTLTTOD (TIMR(1), TBEG)
      IF (TBEG.EQ.0.0) THEN
         CALL ARRSTAT ('Vis/TIME', ' ')
         CALL DATGETR ('Vis/TIME', 'ARRMIN', TBEG, 1, NDUMMY)
      END IF
      CALL UTLTTOD (TIMR(5), TEND)
      IF (TEND.EQ.0.0) THEN
         CALL ARRSTAT ('Vis/TIME', ' ')
         CALL DATGETR ('Vis/TIME', 'ARRMAX', TEND, 1, NDUMMY)
      END IF
      TIME(1) = TBEG
      TIME(2) = TEND
      CALL VISSEL ('Vis','OBS/I', TIME, UVLIMITS, NSEL)
      IF (NSEL.NE.0) THEN
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities for time: ')
         CALL STRAPPEN (MESSAGE, STRTIMC(TIME(1))//' to '//
     1      STRTIMC(TIME(2)))
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No valid data', 'W')
         GO TO 999
      END IF
C
C Fill in model data
C
      CALL VISPOINT ('Vis', 'OBS/I', 'MOD/I', ZFLUX)
      CALL DATPUTR ('Vis', 'TINT', TAVG, 1)
C
C Now self-calibrate data
C
      CALL MSGPUT ('Self-calibration', 'I')
      CALL VISSCAL ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', 'AMPPHI')
C
      IF (GLFILE.NE.' ') THEN
         CALL TXTOPEN ('Gains', GLFILE, 'WRITE')
         CALL CALLIST ('Gains', 'Vis', 'OBS/I')
         CALL TXTCLOSE ('Gains')
      END IF
C
      IF (PLFILE.NE.' ') THEN
         CALL VISANTPO ('Vis', 'OBS/I')
         CALL CALPLOT (PLFILE, 'Vis', 'OBS/I')
      END IF
C
C Start of loop over time
C
      TIME(1) = TBEG
      TIME(2) = MIN(TBEG+TAVG/(24*60*60), TEND)
      NIMAGE = 0
C
      CALL IMGMAKE ('Vis/OBS/I', CELLSIZE, IMSIZE, SHIFT, 'R',
     1   'Initial Dirty')
      CALL DATCREAT ('Gridded Vis')
      CALL FFTCONJA ('Initial Dirty', 'Gridded Vis', DIR, 0)
      CALL IMGCLONE ('Gridded Vis', 'XFR')
C
      CALL ARRCOPY ('Vis/OBS/I/WT', 'Vis/OBS/I/OLDWT')
C
 100  CONTINUE
      CALL VISSEL ('Vis','OBS/I', TIME, UVLIMITS, NSEL)
      IF (ERROR) GO TO 999
      IF (NSEL.EQ.0) THEN
         WRITE (MESSAGE, 1000) NSEL
         CALL STRAPPEN (MESSAGE, STRTIMC(TIME(1))//' to '//
     1      STRTIMC(TIME(2)))
         CALL MSGPUT (MESSAGE, 'I')
         GO TO 110
      ELSE
         NIMAGE = NIMAGE + 1
         WRITE (MESSAGE, 1000) NSEL
         CALL STRAPPEN (MESSAGE, STRTIMC(TIME(1))//' to '//
     1      STRTIMC(TIME(2)))
         CALL MSGPUT (MESSAGE, 'I')
         LIMAGE = STRINT (NIMAGE)
         CALL MSGPUT ('Making and cleaning image '//LIMAGE, 'I')
      END IF
C
C Now reweight and grid
C
C      CALL GRDUWT ('Vis', 'OBS/I', 'XFR')
      CALL VISGRID ('Vis', 'OBS/I', 'Gridded Vis', .FALSE.)
      CALL VISGRID ('Vis', 'OBS/I', 'XFR', .TRUE.)
C
C Now Fourier transform gridded data and weights
C
      CALL IMGFFT ('Gridded Vis', 'Initial Dirty')
      CALL IMGGRIDC ('Initial Dirty', 'Initial Dirty', 'CORRECT')
      CALL IMGFFT ('XFR', 'PSF')
      CALL IMGGRIDC ('PSF', 'PSF','CORRECT')
C
C ************************************************************
C
C                         CLEAN section
C
C ************************************************************
C
      IF (NIMAGE.EQ.1) THEN
         CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
         CALL USRGETC ('Components', CLNFILE, 1, NDUMMY)
         CALL USRGETC ('CLEAN', SCLNFILE, 1, NDUMMY)
         IF (ERROR) GO TO 999
         CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
         CALL ERRCANCE
         CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
         CALL ERRCANCE
         CALL DATCREAT ('Window')
         CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
         CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
         CALL USRGETI ('Niter', NITER, 1, NDUMMY)
         CALL USRGETR ('Flux', FLUX, 1, NDUMMY)
         CALL USRGETR ('Gain', GAIN, 1, NDUMMY)
         CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
         CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
         IF (ERROR) GO TO 999
      END IF
C
C Subsection the dirty image
C
      CALL IMGSUBSE ('Initial Dirty', 'Dirty', 'Window')
C
C Use a null CLEAN box for the moment
C
      IF (NIMAGE.EQ.1)
     $   CALL FILMASGE ('Box', ' ', 'Dirty')
C
      IF (NIMAGE.EQ.1) THEN
         CALL IMGCLONE ('Dirty', 'Components')
         CALL IMGCLONE ('Dirty', 'Residual')
      END IF
      CALL ARRSETCO ('Components', 0.0, 0.0)
C
C Call main routine
C
      CALL DATPUTI ('Components', 'NITER', NITER, 1)
      CALL DATPUTR ('Components', 'FLUX', FLUX, 1)
      CALL DATPUTR ('Components', 'GAIN', GAIN, 1)
      CALL DATPUTR ('Components', 'TFLUX', 0.0, 1)
      CALL IMGCLEAN ('Dirty', 'PSF', 'Components', 'Residual', 'Box')
      CALL DATGETI ('Components', 'NITER', ANITER, 1, NDUMMY)
      CALL DATGETR ('Components', 'FLUX', MAXRES, 1, NDUMMY)
      CALL DATGETR ('Components', 'TFLUX', TFLUX, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
      WRITE (MESSAGE, 1100) ANITER, MAXRES
 1100 FORMAT ('Subtracted ',I6,' components: maximum residual ',
     1  1PE12.4,' Jy/beam')
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1200) TFLUX
 1200 FORMAT ('Total flux = ', 1PE12.4,' Jy')
      CALL MSGPUT (MESSAGE, 'I')
C
C Now smooth if required
C
      IF (BEAM(1).GT.0.0) THEN
         IF (NIMAGE.EQ.1) THEN
            CALL IMGCLONE ('Components', 'Clean')
         END IF
C         
         CALL IMGSMOOT ('Components', BEAM, 'Clean', 'Swork')
         CALL IMGP2PB ('Clean', BEAM, 'Clean')
         CALL ARRLC ('Clean', 1.0, 'Residual', 1.0, 'Clean')
         CALL DATPUTC ('Clean', 'BUNIT', 'JY/BEAM', 1)
         NUMBFILE = SCLNFILE
         CALL STRAPPE2 (NUMBFILE, '.', LIMAGE)
         CALL FILIMGPU ('Clean', NUMBFILE, ' ')
      END IF
C
      IF (CLNFILE.NE.' ') THEN
         CALL DATPUTI ('Components', 'NITER', NITER, 1)
         CALL DATPUTC ('Components', 'BUNIT', 'JY/PIXEL', 1)
         NUMBFILE = CLNFILE
         CALL STRAPPE2 (NUMBFILE, '.', LIMAGE)
         CALL FILIMGPU ('Components', NUMBFILE, ' ')
      END IF
      IF (RESFILE.NE.' ') THEN
         CALL DATPUTC ('Residual', 'BUNIT', 'JY/BEAM', 1)
         NUMBFILE = RESFILE
         CALL STRAPPE2 (NUMBFILE, '.', LIMAGE)
         CALL FILIMGPU ('Residual', NUMBFILE, ' ')
      END IF
C
C Now loop back for more if neccessary
C
 110  CONTINUE
      CALL ARRCOPY ('Vis/OBS/I/OLDWT', 'Vis/OBS/I/WT')
C
      TIME(1) = TIME(2)
      TIME(2) = TIME(2) + TAVG/(24*60*60)
      IF (TIME(2).LE.TEND) GO TO 100
C
 999  CONTINUE
      END
