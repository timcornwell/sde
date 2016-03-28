C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)timemap.f	1.4	 11/9/92
C
      SUBROUTINE SDEMAIN
C
C Program to map and clean visibility data
C
C Audit trail:
C	If only one image then don't number them.
C				T.J.Cornwell	Feb 13 1989
C	Assemble into cube: labelled in time
C				T.J.Cornwell	Feb 22 1989
C	Cleaned up a bit
C				T.J.Cornwell	April 2 1990
C       This is a revised version of MAPPER modified for R. Hjellming
C       by 1) Writing clean components (if requested)
C          2) Imaging on uniform intervals and no blank maps
C          3) Reading AC and BD data into the same map 
C                               R.T.Duquet      April 15 1990  
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIMEMAP')
C
      LOGICAL		ONLYONE, FIRST, GOTBD
      INTEGER 		NDUMMY,  DIR, NIMAGE, TIMR(8), MAXNIMG,
     $                  NSEL, NSEL1, NSEL2,     
     $			NNINT, NNINT1, NITER, ANITER, NX, NY, IMSIZE(3),
     $			BLC (SYSMXDIM), TRC (SYSMXDIM),
     $			NAX, NAXIS(SYSMXDIM)
      REAL		CELLSIZE(3), TIME(2), UVLIMITS(2), 
     $   		TAVG, TAVG0, SHIFT(3), X, FLUX, TFLUX, GAIN,
     $   		MAXRES, BFACT, BEAM(4), FACT, DATFGETR, FOV,
     $                  TBEG, TEND, TBEG1, TINT1, TEND1 
      CHARACTER*6	LIMAGE, STRINT
      CHARACTER*11	STRTIMC
      CHARACTER*(SYSMXNAM)     CLNFILE, RESFILE, VISFILE1, VISFILE2, 
     1			SCLNFILE, NUMBFILE, CUBFILE, CCFILE, SPACING,
     2    		BOXFILE
C==================================================================
C
      CALL MSGWELCO
     $     ('I am a version of MAPPER which uses both AC and BD data')
      CALL USRCTL
      FIRST = .TRUE.
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
      TAVG0 = TAVG
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETC ('VisAC', VISFILE1, 1, NDUMMY)
      CALL USRGETC ('VisBD', VISFILE2, 1, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 3, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('Spacing', SPACING, 1, NDUMMY)
C
C Now get relevant files
C
      CALL VISGET ('VisAC', VISFILE1, 'I', '*', ' ')
      IF (VISFILE2 .NE. ' ') THEN
         GOTBD = .TRUE.
         CALL VISGET ('VisBD', VISFILE2, 'I', '*', ' ')
      ELSE
         GOTBD = .FALSE.
      ENDIF
C
C Start of loop over time
C
      CALL ARRSTAT ('VisAC/TIME', ' ')
      CALL DATGETR ('VisAC/TIME', 'ARRMIN', TIME(1), 1, NDUMMY)
      CALL DATGETR ('VisAC/TIME', 'ARRMAX', TIME(2), 1, NDUMMY)
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
      IF (SPACING(1:1) .NE. 'R') THEN
         IF (TAVG.GT.0.0) THEN
            MAXNIMG = INT((TEND-TBEG)/(TAVG/(24*3600)))
            WRITE (MESSAGE, 1800) MAXNIMG
 1800       FORMAT ('There are ',I6,' time slots')
            CALL MSGPUT (MESSAGE, 'I')
         ELSE 
            MAXNIMG = 1
            WRITE (MESSAGE, 1810) 
 1810       FORMAT ('There is only one time slot')
            CALL MSGPUT (MESSAGE, 'I')
         END IF
      END IF
      IF (TIME(2).GT.TEND) THEN
         ONLYONE = .TRUE.
         TIME(2) = TEND
      ELSE
         ONLYONE = .FALSE.
      END IF
      NIMAGE = 0
      NNINT = MAXNIMG
C
C If spacing is RAGGED check number of occupied time slots 
C
      IF (ONLYONE .EQV. .FALSE. .AND. SPACING(1:1) .EQ. 'R') THEN
         CALL ARRNBOX2('VisAC/TIME','VisAC/OBS/I/WT',TAVG/(24*3600),
     1                  TBEG, TEND, NNINT)
         WRITE(MESSAGE,1820) NNINT
 1820    FORMAT('There are ',I6,' occupied time slots') 
         CALL MSGPUT (MESSAGE, 'I')
      END IF 
C
C Clone input image
C
      CALL IMGMAKE ('VisAC/OBS/I', CELLSIZE, IMSIZE, SHIFT, 'R',
     1   'InitialDirty')
      CALL IMGCLONE ('InitialDirty', 'PSF')
      CALL DATCREAT ('GriddedVis1')
      CALL FFTCONJA ('InitialDirty', 'GriddedVis1', DIR, 0)
      IF (GOTBD) THEN
         CALL DATCREAT ('GriddedVis2')
         CALL FFTCONJA ('InitialDirty', 'GriddedVis2', DIR, 0)
      END IF
C
C Re-weight on the basis of all the data
C
      IF (FOV.NE.0.0) THEN
         CALL MSGPUT ('Uniform weighting', 'I')
         CALL DATPUTR ('GriddedVis1', 'WTFOV', FOV, 1)
         CALL GRDUWT ('VisAC', 'OBS/I', 'GriddedVis1')
         IF (GOTBD) THEN
            CALL DATPUTR ('GriddedVis2', 'WTFOV', FOV, 1)
            CALL GRDUWT ('VisBD', 'OBS/I', 'GriddedVis2')
         END IF
      END IF
C
C Save the new weights for later
C
      CALL ARRCOPY ('VisAC/OBS/I/WT', 'VisAC/OBS/I/OLDWT')
      IF (GOTBD) THEN
         CALL ARRCOPY ('VisBD/OBS/I/WT', 'VisBD/OBS/I/OLDWT')
      END IF
C
C If RAGGED then find the first set of time slots
C
      IF (ONLYONE .EQV. .FALSE. .AND. SPACING(1:1) .EQ. 'R') THEN
         TBEG1 = TBEG
         TEND1 = TEND
         CALL ARRNBOX3('VisAC/TIME','VisAC/OBS/I/WT',TAVG0/(24*3600),
     1                  TBEG1, TINT1, TEND1, NNINT1)
         TIME(1) = TBEG1
         TIME(2) = TBEG1 + TINT1
         TAVG = TINT1 * 24 * 3600
         WRITE(MESSAGE,9000) NNINT1
 9000    FORMAT('First ',I3,' RAGGED slots: ')
         CALL STRAPPEN(MESSAGE,STRTIMC(TBEG1)//' to '//
     1                 STRTIMC(TEND1)//' step '//STRTIMC(TINT1))
         CALL MSGPUT(MESSAGE,'I')
      END IF
C
C Main loop
C
 100  CONTINUE
      NSEL1 = 0
      NSEL2 = 0
      CALL VISSEL ('VisAC','OBS/I', TIME, UVLIMITS, NSEL1)
      IF (ERROR) GO TO 999
      IF (GOTBD) THEN
         CALL VISSEL ('VisBD','OBS/I', TIME, UVLIMITS, NSEL2)
         IF (ERROR) GO TO 999
      END IF
      NSEL = NSEL1 + NSEL2
      IF (NSEL.EQ.0) THEN
         WRITE (MESSAGE, 1000) NSEL1, NSEL2
 1000    FORMAT ('Selected ',I7,' and ',I7,' visibilities for time: ')
         CALL STRAPPEN (MESSAGE, STRTIMC(TIME(1))//' to '//
     1      STRTIMC(TIME(2)))
         CALL MSGPUT (MESSAGE, 'I')
         IF (SPACING(1:1) .NE. 'R') NIMAGE = NIMAGE + 1
         GO TO 110
      ELSE
         NIMAGE = NIMAGE + 1
         WRITE (MESSAGE, 1000) NSEL1, NSEL2
         CALL STRAPPEN (MESSAGE, STRTIMC(TIME(1))//' to '//
     1      STRTIMC(TIME(2)))
         CALL MSGPUT (MESSAGE, 'I')
         X = (TIME(1) + TIME(2)) / 2.0
         WRITE (MESSAGE, 1001) 
 1001    FORMAT('Average time is: ')
         CALL STRAPPEN(MESSAGE,STRTIMC(X))
         CALL MSGPUT(MESSAGE,'I')
         LIMAGE = STRINT (NIMAGE)
         CALL MSGPUT ('Making and cleaning image '//LIMAGE, 'I')
      END IF
C
C Watch out for limit
C
      IF (NIMAGE .GT. NNINT) THEN
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 'Too many time slots used')
         GO TO 999 
      END IF  
C
C Now make images
C
      IF (GOTBD) THEN
         CALL VIS2IMG2 ('VisAC', 'VisBD', 'OBS/I', 'PSF',
     $                  'GriddedVis1', 'GriddedVis2', .TRUE.)
         CALL VIS2IMG2 ('VisAC', 'VisBD', 'OBS/I', 'InitialDirty',
     $                  'GriddedVis1', 'GriddedVis2', .FALSE.)
      ELSE
         CALL VISTOIMG('VisAC', 'OBS/I', 'PSF', 'GriddedVis1', .TRUE.)
         CALL VISTOIMG('VisAC', 'OBS/I', 'InitialDirty',
     $                  'GriddedVis1', .FALSE.)
      END IF
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
      IF (FIRST) THEN
         CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
         CALL USRGETC ('Components', CLNFILE, 1, NDUMMY)
         CALL USRGETC ('Cube', CUBFILE, 1, NDUMMY)
         CALL USRGETC ('CCfile', CCFILE, 1, NDUMMY)
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
      IF(FIRST) THEN
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
      IF (FIRST) THEN
         CALL IMGCLONE ('Dirty', 'Components')
         CALL IMGCLONE ('Dirty', 'Residual')
         CALL IMGCLONE ('Components', 'Clean')
         CALL HISINPUT ('Clean')
         IF(CUBFILE.NE.' ') THEN
            IF(SPACING(1:1) .NE. 'R') THEN
               CALL IMGCUBE ('Clean', 'Cube', MAXNIMG, 'TIME',
     1              DBLE(TBEG), 1.0, TAVG/(24*3600), 0.0)
            ELSE
               CALL IMGCUBE ('Clean', 'Cube', NNINT, 'RAGGED',
     1              DBLE(TBEG), 1.0, 1.0, 0.0)
               CALL CRDLIST('Cube')
            END IF
            CALL ARRSETCO('Cube',0.0,0.0)
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
      CALL IMGSMOOT ('Components', BEAM, 'Clean', 'GriddedVis1')
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
C Write CC file if required
C
      IF (CCFILE .NE. ' ') THEN
         WRITE(STRBUF,2000) CCFILE
 2000    FORMAT('/ CC list for image ',A)
         WRITE(MESSAGE,2001) NIMAGE
 2001    FORMAT('.',I3)
         CALL STRAPPEN(STRBUF,MESSAGE)
         CALL IMGCLIS('Components', CCFILE, STRBUF)
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
      FIRST = .FALSE.
      CALL ARRCOPY ('VisAC/OBS/I/OLDWT', 'VisAC/OBS/I/WT')
      IF (GOTBD) THEN
         CALL ARRCOPY ('VisBD/OBS/I/OLDWT', 'VisBD/OBS/I/WT')
      END IF
C
      TIME(1) = TIME(2)
      TIME(2) = TIME(2) + TAVG/(24*60*60)
C
C If RAGGED and we are about to process the next set of data then find
C the appropriate time slots
C
      IF (ONLYONE .EQV. .FALSE. .AND. SPACING(1:1) .EQ. 'R'
     1    .AND. TIME(1) .GE. TEND1 .AND. TIME(2) .LE. TEND) THEN
         TBEG1 = TIME(1) + TINT1 / 2.0
         TEND1 = TEND
         CALL ARRNBOX3('VisAC/TIME','VisAC/OBS/I/WT',TAVG0/(24*3600),
     1                  TBEG1, TINT1, TEND1, NNINT1)
         TIME(1) = TBEG1
         TIME(2) = TBEG1 + TINT1
         TAVG = TINT1 * 24 * 3600
         WRITE(MESSAGE,3000)
 3000    FORMAT(' ')
         CALL MSGPUT(MESSAGE,'I')
         WRITE(MESSAGE,3001) NNINT1
 3001    FORMAT('Next ',I3,' RAGGED slots: ')
         CALL STRAPPEN(MESSAGE,STRTIMC(TBEG1)//' to '//
     1                 STRTIMC(TEND1)//' step '//STRTIMC(TINT1))
         CALL MSGPUT(MESSAGE,'I')
      END IF
      IF (TIME(2).LE.TEND) GO TO 100
C
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
 999  CONTINUE
      END








