C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)1957.f	1.4	 7/20/92
C
      SUBROUTINE SDEMAIN
C
CD Program to map and clean visibility data building a cube according
C to phase of some cyclic phenomenom.
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Changed to Boxed IMGCLEAN
C					T.J. Cornwell June 1 1992
C	Boxed CLEAN tweaked a little
C					D.S.Briggs	June 10, 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = '1957')
C
      REAL		CELLSIZE(3), TIME(2), UVLIMITS(2), 
     2			TBEG, TEND, DATFGETR
      REAL		PHSTEP, PHBEG, PHEND, PHASE(2), TZPHASE, PERIOD
      REAL		PHRANGE(2), LST(2), ILST(8)
      INTEGER		ITZPHASE(4)
      INTEGER 		NDUMMY,  DIR, NSEL, NIMAGE, TIMR(8), MAXNIMG
      CHARACTER*(SYSMXNAM)	VISFILE
      INTEGER		NITER, ANITER, NX, NY, IMSIZE(3),
     2			BLC (SYSMXDIM), TRC (SYSMXDIM)
      REAL		FLUX, TFLUX, GAIN, MAXRES, BEAM(4),
     1			FOV
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), 
     1			ROTA(SYSMXDIM), SHIFT(3)
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	CLNFILE, RESFILE,
     1			SCLNFILE, NUMBFILE, CUBFILE, PCUBFILE, BOXFILE
      CHARACTER*6	LIMAGE, STRINT
      CHARACTER*11	STRTIMC
      LOGICAL		ONLYONE, FSEL, FIRST
      EXTERNAL          FSEL
      COMMON		/FSELCOM/ PHASE, TZPHASE, PERIOD, LST
C
      DATA		SHIFT	/3*0.0/
      DATA		FIRST	/.TRUE./
C==================================================================
C
      CALL MSGWELCO('I image 1957 at P-band')
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
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETR ('PhaseStep', PHSTEP, 1, NDUMMY)
      CALL USRGETR ('Phaserange', PHRANGE, 2, NDUMMY)
      CALL USRGETR ('TZPhase', ITZPHASE, 4, NDUMMY)
      CALL USRGETR ('Period', PERIOD, 1, NDUMMY)
      CALL USRGETR ('LSTrange', ILST, 8, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
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
      IF(TEND.NE.0.0) THEN
         TEND = MIN(TIME(2), TEND)
      ELSE
         TEND = TIME(2)
      END IF
C
      TIME(1) = TBEG
      TIME(2) = TEND
C
      CALL UTLTTOD (ILST(1), LST(1))
      CALL UTLTTOD (ILST(5), LST(2))
      IF((LST(1).NE.0.0).AND.(LST(2).NE.0.0)) THEN
         WRITE (MESSAGE, 1790) LST(1), LST(2)
 1790    FORMAT ('Selected LST range is ',F7.2,' to ',F7.2,' hours')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No LST range specified', 'I')
      END IF
C
      CALL UTLTTOD (ITZPHASE, TZPHASE)
      PHBEG = PHRANGE(1)
      PHEND = PHRANGE(2)
      IF (PHSTEP.GT.0.0) THEN
         MAXNIMG = INT((PHEND-PHBEG)/PHSTEP)
         WRITE (MESSAGE, 1800) MAXNIMG
 1800    FORMAT ('There are ',I6,' phase slots')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         MAXNIMG = 1
         WRITE (MESSAGE, 1810) 
 1810    FORMAT ('There is only one phase slot')
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
     1   'Initial Dirty')
      CALL DATCREAT ('Gridded Vis')
      CALL FFTCONJA ('Initial Dirty', 'Gridded Vis', DIR, 0)
      CALL IMGCLONE ('Gridded Vis', 'XFR')
C
C Do initial selction
C
      CALL VISSEL ('Vis','OBS/I', TIME, UVLIMITS, NSEL)
      WRITE (MESSAGE, 2000) NSEL
 2000 FORMAT ('Selected ',I7,' visibilities')
      CALL MSGPUT (MESSAGE, 'I')
      CALL VISATF ('Vis')
C
C Re-weight on the basis of all the data
C
      IF(FOV.NE.0.0) THEN
         CALL GRDUWT ('Vis', 'OBS/I', 'XFR')
      END IF
C
C Save the new weights for later
C
      CALL ARRCOPY ('Vis/OBS/I/WT', 'Vis/OBS/I/OLDWT')
C
      PHASE(1) = PHBEG
      PHASE(2) = PHBEG + PHSTEP
 100  CONTINUE
      CALL VISFSEL ('Vis', 'OBS/I', FSEL, NSEL)
      IF (ERROR) GO TO 999
      IF (NSEL.EQ.0) THEN
         WRITE (MESSAGE, 1000) NSEL, PHASE(1), PHASE(2)
 1000    FORMAT ('Selected ',I7,' visibilities for phase: ', F7.2,
     $      ' to ', F7.2,' turns')
         CALL MSGPUT (MESSAGE, 'I')
         NIMAGE = NIMAGE + 1
         GO TO 110
      ELSE
         NIMAGE = NIMAGE + 1
         WRITE (MESSAGE, 1000) NSEL, PHASE(1), PHASE(2)
         CALL MSGPUT (MESSAGE, 'I')
         LIMAGE = STRINT (NIMAGE)
         CALL MSGPUT ('Making and cleaning image '//LIMAGE, 'I')
      END IF
C
C Now grid data
C
      CALL VISGRID ('Vis', 'OBS/I', 'Gridded Vis', .FALSE.)
      CALL VISGRID ('Vis', 'OBS/I', 'XFR', .TRUE.)
C
C Now Fourier transform gridded data and weights
C
      CALL IMGFFT ('Gridded Vis', 'Initial Dirty')
      CALL DATPUTC ('Initial Dirty', 'CFTYPE', 'SF', 1)
      CALL IMGGRIDC ('Initial Dirty', 'Initial Dirty', 'CORRECT')
      CALL IMGFFT ('XFR', 'PSF')
      CALL DATPUTC ('PSF', 'CFTYPE', 'SF', 1)
      CALL IMGGRIDC ('PSF', 'PSF', 'CORRECT')
      IF (ERROR) GO TO 999
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
         CALL USRGETC ('PCube', PCUBFILE, 1, NDUMMY)
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
         CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
         IF (ERROR) GO TO 999
      END IF
C
C Make window
C
      IF(FIRST) THEN
         CALL DATCREAT ('Window')
         CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
         NX = TRC(1) - BLC(1) + 1
         NY = TRC(2) - BLC(2) + 1
         IF ((2*NX.GE.IMSIZE(1)).OR.(2*NY.GE.IMSIZE(2))) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Clean window too big')
            GO TO 999
         END IF
         CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
      END IF
      CALL IMGSUBSE ('Initial Dirty', 'Dirty', 'Window')
      IF (FIRST) THEN
         CALL FILMASGE ('Box', BOXFILE, 'Dirty')
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
      TFLUX = 0.0
      CALL DATPUTR ('Components', 'TFLUX', TFLUX, 1)
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
         IF (FIRST) THEN
            CALL IMGCLONE ('Components', 'Clean')
            CALL HISINPUT ('Clean')
            CALL IMGCUBE ('Clean', 'Cube', MAXNIMG, 'PHASE',
     1         DBLE(PHBEG+0.5*PHSTEP), 1.0, PHSTEP, 0.0)
            IF (PCUBFILE.NE.' ') THEN
               CALL IMGCUBE ('PSF', 'PCube', MAXNIMG, 'PHASE',
     1            DBLE(PHBEG+0.5*PHSTEP), 1.0, PHSTEP, 0.0)
            END IF
            FIRST = .FALSE.
         END IF
         CALL CRDGET ('Clean', NAX, TYPE, NAXIS, RVAL, RPIX, 
     1      DELT, ROTA)
         IF (ERROR) GOTO 999
C         
         CALL IMGSMOOT ('Components', BEAM, 'Clean', 'Swork')
         CALL IMGP2PB ('Clean', BEAM, 'Clean')
         CALL ARRADD ('Clean', 'Residual', 'Clean')
         IF(SCLNFILE.NE.' ') THEN
            NUMBFILE = SCLNFILE
            IF(.NOT.ONLYONE) CALL STRAPPE2 (NUMBFILE, '.', LIMAGE)
            CALL FILIMGPU ('Clean', NUMBFILE, ' ')
         END IF
      END IF
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
      IF (PCUBFILE.NE.' ') THEN
         BLC(1) = 1
         BLC(2) = 1
         TRC(1) = IMSIZE(1)
         TRC(2) = IMSIZE(2)
         BLC(3) = NIMAGE
         TRC(3) = NIMAGE
         CALL DATCREAT ('PCubewindow')
         CALL DATPUTI ('PCubewindow', 'BLC', BLC, SYSMXDIM)
         CALL DATPUTI ('PCubewindow', 'TRC', TRC, SYSMXDIM)
         CALL ARRINSER ('PSF', 'PCube', 'PCubewindow')
         WRITE (MESSAGE, 1901) NIMAGE
 1901    FORMAT ('Image ',I4, ' ')
         CALL STRAPPEN (MESSAGE, STRTIMC(TIME(1))//' to '//
     1      STRTIMC(TIME(2)))
         CALL HISPUT ('Cube', MESSAGE)
      END IF
      IF (CLNFILE.NE.' ') THEN
         CALL DATPUTI ('Components', 'NITER', NITER, 1)
         CALL DATPUTC ('Components', 'BUNIT', 'JY/PIXEL', 1)
         NUMBFILE = CLNFILE
         IF(.NOT.ONLYONE) CALL STRAPPE2 (NUMBFILE, '.', LIMAGE)
         CALL FILIMGPU ('Components', NUMBFILE, ' ')
      END IF
      IF (RESFILE.NE.' ') THEN
         CALL DATPUTC ('Residual', 'BUNIT', 'JY/BEAM', 1)
         NUMBFILE = RESFILE
         IF(.NOT.ONLYONE) CALL STRAPPE2 (NUMBFILE, '.', LIMAGE)
         CALL FILIMGPU ('Residual', NUMBFILE, ' ')
      END IF
C
C Now loop back for more if neccessary

 110  CONTINUE
      CALL ARRCOPY ('Vis/OBS/I/OLDWT', 'Vis/OBS/I/WT')
C
      PHASE(1) = PHASE(1) + PHSTEP
      PHASE(2) = PHASE(2) + PHSTEP
      IF (PHASE(2).LE.PHEND) GO TO 100
C
C Write cube
C
      IF(CUBFILE.NE.' ') THEN
         IF(NITER.GT.0) THEN
            CALL DATPUTR ('Cube', 'BMAJ', BEAM(1)/3600.0, 1)
            CALL DATPUTR ('Cube', 'BMIN', BEAM(2)/3600.0, 1)
            CALL DATPUTR ('Cube', 'BPA',  BEAM(3), 1)
            CALL DATPUTR ('Cube', 'BZ', BEAM(4)/3600.0, 1)
         END IF
         CALL DATPUTC ('Cube', 'BUNIT', 'JY/BEAM', 1)
         CALL FILIMGPU ('Cube', CUBFILE, ' ')
      END IF
      IF(PCUBFILE.NE.' ') THEN
         IF(NITER.GT.0) THEN
            CALL DATPUTR ('PCube', 'BMAJ', BEAM(1)/3600.0, 1)
            CALL DATPUTR ('PCube', 'BMIN', BEAM(2)/3600.0, 1)
            CALL DATPUTR ('PCube', 'BPA',  BEAM(3), 1)
            CALL DATPUTR ('PCube', 'BZ', BEAM(4)/3600.0, 1)
         END IF
         CALL DATPUTC ('PCube', 'BUNIT', 'JY/BEAM', 1)
         CALL FILIMGPU ('PCube', PCUBFILE, ' ')
      END IF
C
 999  CONTINUE
      END
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      LOGICAL FUNCTION FSEL (U,V,W,TIME,BASL)
C
CD Function to select a visibility data point.
C
CS Arguments: FSEL (U,V,W,TIME,BASL)
CS
CS	U,V,W	REAL	input	U,v,w coordinates in wavelengths
CS	TIME	REAL	input	time in days
CS	BASL	REAL	input	Baseline
CA
CA Audit trail:
CA	Original version: Audit trail comments go on this line
CA	and successive lines
CA				T.J.Cornwell	Jan 5 1989
CE
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL		U, V, W, TIME, BASL
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'FSEL')
C
      REAL		PHASE(2), TZPHASE, PERIOD, PPERIOD, 
     $			ALST, LST(2)
      REAL		DELTAT
      INTEGER		NUMLIST
      PARAMETER(DELTAT =  0.0002789)
	PARAMETER(PPERIOD = 0.3819560)
	PARAMETER(NUMLIST = 24)
      COMMON		/FSELCOM/ PHASE, TZPHASE, PERIOD, LST
C=======================================================================
C
C If an error on input then exit immediately
C
C
	integer arrayno, icount
	real orbphs,qphase(NUMLIST),qlst(NUMLIST),uttime,utdif
        character*10	strtimc

c	qphase gives orbital phase at UT date + 0.25 days

	data qphase /0.2809, 0.0916, 0.2378, 0.2378, 0.3839, 0.5171, 
     1               0.2809, 0.0916, 0.2378, 0.2378, 0.3839, 0.5171, 
     2               0.7321, 0.7879, 0.9120, 0.8781, 0.0351, 0.9784, 
     3               0.7321, 0.7879, 0.9120, 0.8781, 0.0351, 0.9784/


	data qlst /18.17, 21.13, 20.93, 20.93, 20.74, 18.30,
     1             18.17, 21.13, 20.93, 20.93, 20.74, 18.30,
     2             19.82, 19.29, 20.47, 19.62, 21.65, 22.18, 
     3             19.82, 19.29, 20.47, 19.62, 21.65, 22.18/

C
C Original L-band data
C
C	data qphase /0.4116, 0.4116, 0.6692, 0.6692, 0.0513,
C     1               0.0513, 0.9281, 0.9281, 0.4808, 0.4808,
C     2               0.4488, 0.4488, 0.2386, 0.2386, 
C     3               0.6208, 0.6208, 0.3851, 0.3851/
        data icount/0/
        save icount

      IF (ERROR) GO TO 999
      ICOUNT=ICOUNT+1
        ARRAYNO = 1 + NINT(100.0*(BASL - FLOAT(INT(BASL))))
	uttime = time - DELTAT

c       find reference phase of arrayno

c       find phase of observation (fiducial point 0.25 + UT midnite)
	utdif = uttime - 0.25
	orbphs = mod(utdif/PPERIOD + qphase(arrayno),1.0)

        if(sysdebug.and.mod(icount,1000).eq.1) then
           write(MESSAGE,*) icount, arrayno, orbphs, ' '
           CALL STRAPPEN (MESSAGE,STRTIMC(TIME))
           CALL MSGPUT (MESSAGE,'D')

        endif

C	write (6,*)'time,i,qphase,orbphs ',time,i,qphase(i),orbphs
C	write (6,*)'uttime,utdif ',uttime,utdif

C
C Select according to orbital phase
C
      IF ((ORBPHS.GT.PHASE(1)).AND.(ORBPHS.LT.PHASE(2))) THEN
         FSEL = .TRUE.
      ELSE
         FSEL = .FALSE.
      END IF
C
C Select according to LST
C
      IF((LST(1).NE.0.0).AND.(LST(2).NE.0.0)) THEN
         ALST=QLST(ARRAYNO)+0.997269566*(24.0*MOD(TIME,1.0)-6.0)
         IF ((ALST.GT.LST(1)).AND.(ALST.LT.LST(2))) THEN
            FSEL = .TRUE.
         ELSE
            FSEL = .FALSE.
         END IF
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

