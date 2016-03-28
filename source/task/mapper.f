C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mapper.f	1.4    6/12/92
C
      SUBROUTINE SDEMAIN
C
CD Program to map and clean visibility data
C
C Audit trail:
C	If only one image then don't number them.
C				T.J.Cornwell	Feb 13 1989
C	Assemble into cube: labelled in time
C				T.J.Cornwell	Feb 22 1989
C	Cleaned up a bit
C				T.J.Cornwell	April 2 1990
C	Soft CLEAN boxes added.
C				D.S.Briggs	June 12 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MAPPER')
C
      LOGICAL		ONLYONE
      INTEGER 		NDUMMY,  DIR, NSEL, NIMAGE, TIMR(8), MAXNIMG,
     $			NITER, ANITER, NX, NY, IMSIZE(3),
     $			BLC (SYSMXDIM), TRC (SYSMXDIM),
     $			NAX, NAXIS(SYSMXDIM)
      REAL		CELLSIZE(3), TIME(2), UVLIMITS(2), TAVG,
     $   		TBEG, TEND, SHIFT(3), FLUX, TFLUX, GAIN,
     $   		MAXRES, BFACT, BEAM(4), FACT, DATFGETR, FOV
      CHARACTER*6	LIMAGE, STRINT
      CHARACTER*12	STRTIMC
      CHARACTER*(SYSMXNAM)	CLNFILE, RESFILE, VISFILE, 
     1			SCLNFILE, NUMBFILE, CUBFILE, BOXFILE
C==================================================================
C
      CALL MSGWELCO('I make and clean images from visibility data')
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
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 3, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
C
C Now get relevant files
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
C
C Start of loop over time
C
      CALL ARRSTAT ('Vis/TIME', ' ')
      CALL DATGETR ('Vis/TIME', 'ARRMIN', TIME(1), 1, NDUMMY)
      CALL DATGETR ('Vis/TIME', 'ARRMAX', TIME(2), 1, NDUMMY)
      MESSAGE = 'Time range in data is '
      CALL STRAPPEN (MESSAGE, STRTIMC(TIME(1))//' to '//
     1      STRTIMC(TIME(2)))
      CALL MSGPUT (MESSAGE, 'I')
      CALL UTLTTOD (TIMR(1), TBEG)
      CALL UTLTTOD (TIMR(5), TEND)
      TBEG = MAX(TIME(1), TBEG)
      TEND = MIN(TIME(2), TEND)
C
      TIME(1) = TBEG
      TIME(2) = TBEG+TAVG/(24*60*60)
      IF (TAVG.GT.0.0) THEN
         MAXNIMG = INT((TEND-TBEG)/(TAVG/(24*3600)))
         WRITE (MESSAGE, 1800) MAXNIMG
 1800    FORMAT ('There are ',I6,' time slots')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         MAXNIMG = 1
         WRITE (MESSAGE, 1810) 
 1810    FORMAT ('There is only one time slot')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
      IF (TIME(2).GT.TEND) THEN
         ONLYONE = .TRUE.
         TIME(2) = TEND
      ELSE
         ONLYONE = .FALSE.
      END IF
      NIMAGE = 0
C
C Clone input image
C
      CALL IMGMAKE ('Vis/OBS/I', CELLSIZE, IMSIZE, SHIFT, 'R',
     1   'InitialDirty')
      CALL IMGCLONE ('InitialDirty', 'PSF')
      CALL DATCREAT ('GriddedVis')
      CALL FFTCONJA ('InitialDirty', 'GriddedVis', DIR, 0)
C
C Re-weight on the basis of all the data
C
      IF (FOV.NE.0.0) THEN
         CALL MSGPUT ('Uniform weighting', 'I')
         CALL DATPUTR ('GriddedVis', 'WTFOV', FOV, 1)
         CALL GRDUWT ('Vis', 'OBS/I', 'GriddedVis')
      END IF
C
C Save the new weights for later
C
      CALL ARRCOPY ('Vis/OBS/I/WT', 'Vis/OBS/I/OLDWT')
C
 100  CONTINUE
      CALL VISSEL ('Vis','OBS/I', TIME, UVLIMITS, NSEL)
      IF (ERROR) GO TO 999
      IF (NSEL.EQ.0) THEN
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities for time: ')
         CALL STRAPPEN (MESSAGE, STRTIMC(TIME(1))//' to '//
     1      STRTIMC(TIME(2)))
         CALL MSGPUT (MESSAGE, 'I')
         NIMAGE = NIMAGE + 1
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
C Now make images
C
      CALL VISTOIMG ('Vis', 'OBS/I', 'PSF', 'GriddedVis', .TRUE.)
      CALL VISTOIMG ('Vis', 'OBS/I', 'InitialDirty', 'GriddedVis',
     $   .FALSE.)
C
C Fit a Gaussian Beam to PSF
C
      IF (BEAM(1) .EQ. 0. ) THEN
         CALL IMGBMSHP( 'PSF' )
         IF (ERROR) GO TO 999
         BEAM(1) =  DATFGETR( 'PSF', 'BMAJ')*3600.0
         BEAM(2) =  DATFGETR( 'PSF', 'BMIN')*3600.0
         BEAM(3) =  DATFGETR( 'PSF', 'BPA')
         BEAM(4) =  DATFGETR( 'PSF', 'BZ')*3600.0
      ENDIF
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
         CALL USRGETC ('Cube', CUBFILE, 1, NDUMMY)
         CALL USRGETC ('CLEAN', SCLNFILE, 1, NDUMMY)
         IF (ERROR) GO TO 999
         CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
         CALL ERRCANCE
         CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
         CALL ERRCANCE
         CALL USRGETC ('Box', BOXFILE, 1, NDUMMY)
         CALL USRGETI ('Niter', NITER, 1, NDUMMY)
         CALL USRGETR ('Flux', FLUX, 1, NDUMMY)
         CALL USRGETR ('Gain', GAIN, 1, NDUMMY)
         CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
         IF (ERROR) GO TO 999
      END IF
C
C Make window
C
      IF(NIMAGE.EQ.1) THEN
         CALL CRDNHALF ('InitialDirty', BLC, TRC)
         CALL DATCREAT ('Window')
         CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
         CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
         NX = TRC(1) - BLC(1) + 1
         NY = TRC(2) - BLC(2) + 1
      END IF
C
C Subsection dirty image
C
      CALL IMGSUBSE ('InitialDirty', 'Dirty', 'Window')
C
C Load a clean box
C
      IF (NIMAGE.EQ.1)
     $   CALL FILMASGE ('Box', BOXFILE, 'Dirty')
C
C Make cube
C
      IF (NIMAGE.EQ.1) THEN
         CALL IMGCLONE ('Dirty', 'Components')
         CALL IMGCLONE ('Dirty', 'Residual')
         CALL IMGCLONE ('Components', 'Clean')
         CALL HISINPUT ('Clean')
         IF(CUBFILE.NE.' ') THEN
            CALL IMGCUBE ('Clean', 'Cube', MAXNIMG, 'TIME',
     1         DBLE(TBEG), 1.0, TAVG/(24*3600), 0.0)
         END IF
      END IF
C
C Call main routine
C
      CALL DATPUTI ('Components', 'NITER', NITER, 1)
      CALL DATPUTR ('Components', 'FLUX', FLUX, 1)
      CALL DATPUTR ('Components', 'GAIN', GAIN, 1)
      TFLUX = 0.0
      CALL DATPUTR ('Components', 'TFLUX', TFLUX, 1)
      CALL ARRSETCO ('Components', 0.0, 0.0)
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
C Now smooth
C
      CALL IMGSMOOT ('Components', BEAM, 'Clean', 'GriddedVis')
      CALL IMGP2PB ('Clean', BEAM, 'Clean')
      CALL ARRLC ('Clean', 1.0, 'Residual', 1.0, 'Clean')
C
C Write as separate files?
C
      IF(SCLNFILE.NE.' ') THEN
         NUMBFILE = SCLNFILE
         IF(.NOT.ONLYONE) CALL STRAPPE2 (NUMBFILE, '.', LIMAGE)
         CALL FILIMGPU ('Clean', NUMBFILE, ' ')
      END IF
C
C Write as cube?
C
      IF (CUBFILE.NE.' ') THEN
         BLC(1) = 1
         BLC(2) = 1
         TRC(1) = NX
         TRC(2) = NY
         BLC(3) = NIMAGE
         TRC(3) = NIMAGE
         CALL DATCREAT ('Cubewindow')
         CALL DATPUTI ('Cubewindow', 'BLC', BLC, SYSMXDIM)
         CALL DATPUTI ('Cubewindow', 'TRC', TRC, SYSMXDIM)
         CALL ARRINSER ('Clean', 'Cube', 'Cubewindow')
         WRITE (MESSAGE, 1900) NIMAGE
 1900    FORMAT ('Image ',I4, ' ')
         CALL STRAPPEN (MESSAGE, STRTIMC(TIME(1))//' to '//
     1      STRTIMC(TIME(2)))
         CALL HISPUT ('Cube', MESSAGE)
      END IF
C
C Write component files?
C
      IF (CLNFILE.NE.' ') THEN
         CALL DATPUTI ('Components', 'NITER', NITER, 1)
         CALL DATPUTC ('Components', 'BUNIT', 'JY/PIXEL', 1)
         NUMBFILE = CLNFILE
         IF(.NOT.ONLYONE) CALL STRAPPE2 (NUMBFILE, '.', LIMAGE)
         CALL FILIMGPU ('Components', NUMBFILE, ' ')
      END IF
C
C Write residuals?
C
      IF (RESFILE.NE.' ') THEN
         CALL DATPUTC ('Residual', 'BUNIT', 'JY/BEAM', 1)
         NUMBFILE = RESFILE
         IF(.NOT.ONLYONE) CALL STRAPPE2 (NUMBFILE, '.', LIMAGE)
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
C Write cube
C
      IF(CUBFILE.NE.' ') THEN
         CALL DATPUTR ('Cube', 'BMAJ', BEAM(1)/3600.0, 1)
         CALL DATPUTR ('Cube', 'BMIN', BEAM(2)/3600.0, 1)
         CALL DATPUTR ('Cube', 'BPA',  BEAM(3), 1)
         CALL DATPUTR ('Cube', 'BZ', BEAM(4)/3600.0, 1)
         CALL DATPUTC ('Cube', 'BUNIT', 'JY/BEAM', 1)
         CALL FILIMGPU ('Cube', CUBFILE, ' ')
      END IF
C
 999  CONTINUE
      END
