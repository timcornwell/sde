C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to map and clean visibility data building a cube according
CD to phase of some cyclic phenomenom.
C
CS Arguments: CALL SDEMAIN
CA Audit trail:
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PULSAR')
C
      REAL		CELLSIZE(3), TIME(2), UVLIMITS(2), TAVG,
     2			TBEG, TEND
      REAL		PHSTEP, PHBEG, PHEND, PHASE(2), TZPHASE, PERIOD
      REAL		PHRANGE(2)
      INTEGER		ITZPHASE(4)
      INTEGER 		NDUMMY,  DIR, NSEL, NIMAGE, TIMR(8), MAXNIMG
      CHARACTER*(SYSMXNAM)	VISFILE
      INTEGER		NITER, ANITER, NX, NY, IMSIZE(3),
     2			BLC (SYSMXDIM), TRC (SYSMXDIM)
      REAL		FLUX, TFLUX, GAIN, MAXRES, BFACT, BEAM(4),
     1			SBEAM(4), FACT, FOV
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), 
     1			ROTA(SYSMXDIM), SHIFT(3)
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	CLNFILE, RESFILE,
     1			SCLNFILE, NUMBFILE, CUBFILE, PCUBFILE
      CHARACTER*6	LIMAGE, STRINT
      CHARACTER*11	STRTIMC
      LOGICAL		ONLYONE, FSEL, FIRST, DOPSF
      EXTERNAL          FSEL
      COMMON		/FSELCOM/ PHASE, TZPHASE, PERIOD
C
      DATA		SHIFT	/3*0.0/
      DATA		FIRST	/.TRUE./
C==================================================================
C
      CALL MSGWELCO('I am a special imager for cyclic phenomena')
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
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
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
      TEND = MIN(TIME(2), TEND)
C
      TIME(1) = TBEG
      TIME(2) = TEND
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
     $      ' to ', F7.2,' degrees')
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
         CALL USRGETI ('Niter', NITER, 1, NDUMMY)
         CALL USRGETR ('Flux', FLUX, 1, NDUMMY)
         CALL USRGETR ('Gain', GAIN, 1, NDUMMY)
         CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
         CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
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
      CALL IMGCLEAN ('Dirty', 'PSF', 'Components', 'Residual')
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
     1         DBLE(PHBEG), 1.0, PHSTEP, 0.0)
            IF (PCUBFILE.NE.' ') THEN
               CALL IMGCUBE ('PSF', 'PCube', MAXNIMG, 'PHASE',
     1            DBLE(PHBEG), 1.0, PHSTEP, 0.0)
            END IF
            FIRST = .FALSE.
         END IF
         CALL CRDGET ('Clean', NAX, TYPE, NAXIS, RVAL, RPIX, 
     1      DELT, ROTA)
         IF (ERROR) GOTO 999
C         
         FACT = SQRT(ATAN(1.0)/LOG(2.0))
         IF (NAXIS(3).NE.1) THEN
            SBEAM(1) = BEAM(1)/ABS(3600.0*DELT(1))
            SBEAM(2) = BEAM(2)/ABS(3600.0*DELT(2))
            SBEAM(3) = BEAM(3)
            SBEAM(4) = BEAM(4)/ABS(3600.0*DELT(3))
            BFACT = FACT**3 * SBEAM(1) * SBEAM(2) * SBEAM(4)
         ELSE
            SBEAM(1) = BEAM(1)/ABS(3600.0*DELT(1))
            SBEAM(2) = BEAM(2)/ABS(3600.0*DELT(2))
            SBEAM(3) = BEAM(3)
            SBEAM(4) = 0.0
            BFACT = FACT**2 * SBEAM(1) * SBEAM(2) 
         END IF
         CALL IMGSMOOT ('Components', SBEAM, 'Clean', 'Swork')
         CALL ARRLC ('Clean', BFACT, 'Residual', 1.0, 'Clean')
         CALL DATPUTR ('Clean', 'BMAJ', BEAM(1)/3600.0, 1)
         CALL DATPUTR ('Clean', 'BMIN', BEAM(2)/3600.0, 1)
         CALL DATPUTR ('Clean', 'BPA',  BEAM(3), 1)
         CALL DATPUTR ('Clean', 'BZ', BEAM(4)/3600.0, 1)
         CALL DATPUTC ('Clean', 'BUNIT', 'JY/BEAM', 1)
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
      REAL		PHASE(2), TZPHASE, PERIOD, APHASE, PPERIOD
      REAL		DELTAT
      INTEGER		NUMLIST
      PARAMETER(DELTAT =  0.0002789)
	PARAMETER(PPERIOD = 0.3819560)
	PARAMETER(NUMLIST = 18)
      COMMON		/FSELCOM/ PHASE, TZPHASE, PERIOD
C=======================================================================
C
C If an error on input then exit immediately
C
C
	integer I, arrayno, icount
	real orbphs,qphase(NUMLIST),uttime,utdif
        character*10	strtimc

c	qphase gives orbital phase at UT date + 0.25 days
	data qphase /0.4116, 0.4116, 0.6692, 0.6692, 0.0513,
     1               0.0513, 0.9281, 0.9281, 0.4808, 0.4808,
     2               0.4488, 0.4488, 0.2386, 0.2386, 
     3               0.6208, 0.6208, 0.3851, 0.3851/
        data icount/0/
        save icount

      IF (ERROR) GO TO 999
      ICOUNT=ICOUNT+1
        ARRAYNO = 1 + NINT(100.0*(BASL - FLOAT(INT(BASL))))
	uttime = time - DELTAT

c       find reference phase of arrayno

c       find phase of observation (fiducial point 0.25 + UT midnite)
	utdif = uttime - 0.25
	orbphs = 360.0*mod(utdif/PPERIOD + qphase(arrayno),1.0)

        if(sysdebug.and.mod(icount,1000).eq.1) then
           write(MESSAGE,*) icount, arrayno, orbphs, ' '
           CALL STRAPPEN (MESSAGE,STRTIMC(TIME))
           CALL MSGPUT (MESSAGE,'D')

        endif

C	write (6,*)'time,i,qphase,orbphs ',time,i,qphase(i),orbphs
C	write (6,*)'uttime,utdif ',uttime,utdif

      IF ((ORBPHS.GT.PHASE(1)).AND.(ORBPHS.LT.PHASE(2))) THEN
         FSEL = .TRUE.
      ELSE
         FSEL = .FALSE.
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

