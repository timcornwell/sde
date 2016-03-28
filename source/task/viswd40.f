C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C  @(#)viswd40.f	1.2 7/22/92
C
      SUBROUTINE SDEMAIN
C
CD Program to perform vis-based maximum entropy deconvolution.
C
C Uses the Wilczek-Drapatz algorithm modified for conjugate gradients 
C in place of Newton Raphson. The data space is taken to be the
C visibility plane.
C
C Audit trail:
C	New task
C					T.J. Cornwell 15 June 1992
C	Added point source option
C					T.J. Cornwell 18 June 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISWD40')
C
      INTEGER		NITER, BITER, ITER, NDUMMY, NAX, IMSIZE(3),
     1			NAXIS(SYSMXDIM), NCONJ, DATADD, TIMR(8),
     $			VISSZ, NSEL, SIGADD, WTADD, I, DIR, IMSZ
      REAL		FLUX, TCHISQ, CHISQ, Z, TOL, LENGTH, SIGMA,
     2			BEAM(4), DATFGETR, MU, NOISE, TFLUX, FOV
      REAL		CELLSIZE(3), TAPER(3), TIME(2), UVLIMITS(2),
     $   		SHIFT(3)
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	WDFILE, DEFFILE, VISFILE, 
     1			CWDFILE, TVFILE, CTIME
      LOGICAL		DATEXIST, FILEXIST, POINT
      LOGICAL		FINISH, T, F
      EXTERNAL		FTRANS, LTRANS
      DATA		T	/.TRUE./
      DATA		F	/.FALSE./
C==================================================================
      CALL MSGWELCO (
     &   'I perform visibility-based maximum entropy deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 3, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('Filter', TAPER, 3, NDUMMY)
      CALL USRGETR ('FOV', FOV, 3, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETC ('Default', DEFFILE, 1, NDUMMY)
      CALL USRGETC ('WD', WDFILE, 1, NDUMMY)
      CALL USRGETC ('CWD', CWDFILE, 1, NDUMMY)
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETI ('Nconj', NCONJ, 1, NDUMMY)
      CALL USRGETR ('Sigma', SIGMA, 1, NDUMMY)
      CALL USRGETR ('Tflux', FLUX, 1, NDUMMY)
      CALL USRGETL ('Point', POINT, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETR ('Tolerance', TOL, 1, NDUMMY)
      CALL USRGETC ('TV', TVFILE, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Now get relevant files
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
C
      IF(DATEXIST('Vis/TIME')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL ('Vis', 'OBS/I', TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No TIME information', 'W')
      END IF
C
C If required fill in Point source info
C
      IF (POINT) THEN
         CALL MSGPUT ('Making image for point source', 'I')
         CALL ARRSETCO ('Vis/OBS/I/VIS', 0.0, 1.0)
      ENDIF
C
C Make output image and Fourier equivalent
C
      CALL IMGMAKE ('Vis/OBS/I', CELLSIZE, IMSIZE, SHIFT, 'R', 'Util')
      NDUMMY = 0
      CALL FFTCONJA ('Util', 'WDVis', DIR, NDUMMY)
      CALL IMGDOUBL ('Util', 'DUtil')
      CALL FFTCONJA ('DUtil', 'DWDVis', DIR, NDUMMY)
C
C Uniform weight the visibility data?
C
      IF (FOV.NE.0.0) THEN
         CALL DATPUTR ('WDVis', 'WTFOV', FOV, 1)
         CALL GRDUWT ('Vis', 'OBS/I', 'WDVis')
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1101) FOV
 1101    FORMAT ('Weighting for fraction of field of view = ',
     1      F7.2)
         CALL MSGPUT (MESSAGE, 'I')
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Weighting completed: '//CTIME, 'I')
      END IF
C
C Apply taper to visibility data?
C
      IF ((TAPER(1).NE.0.0).OR.(TAPER(2).NE.0.0)) THEN
         CALL VISTAPER ('Vis', 'OBS/I', TAPER, 'OBS/I')
         IF (ERROR) GO TO 999
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Filtering: '//CTIME, 'I')
      END IF
      IF (ERROR) GO TO 999
C
C Make PSF and fit Gaussian?
C
      IF (BEAM(1).EQ.0.0) THEN
         CALL VISTOIMG ('Vis', 'OBS/I', 'Util', 'DWDVis', .TRUE.)
         CALL IMGBMSHP('Util')
         IF (ERROR) GO TO 999
         BEAM(1) = DATFGETR('Util','BMAJ')*3600.0
         BEAM(2) = DATFGETR('Util','BMIN')*3600.0
         BEAM(3) = DATFGETR('Util','BPA')
         BEAM(4) = DATFGETR('Util','BZ')*3600.0
      ENDIF
C
C Clone various arrays to store working info. Note that the MWD routines
C think that these arrays are REAL. 
C
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
      CALL VISCLONE ('Vis', 'OBS', 'I', 'Lambda')
      CALL VISCLONE ('Vis', 'OBS', 'I', 'Sigma')
      CALL VISCLONE ('Vis', 'OBS', 'I', 'Work1')
      CALL VISCLONE ('Vis', 'OBS', 'I', 'Work2')
      CALL VISCLONE ('Vis', 'OBS', 'I', 'Work3')
C
C Fill in sigma array with specified value modified by weight in
C weights array. Then reset weights to be unity since we don't want
C weighting in the transform from visibility to image planes.
C
      CALL DATGETAR ('Vis/Lambda/I/WT', NAX, NAXIS, ATYPE, WTADD)
      CALL DATGETAR ('Vis/Sigma/I/VIS', NAX, NAXIS, ATYPE, SIGADD)
      VISSZ = 2*NAXIS(1)
      IF(SIGMA.EQ.0.0) THEN
         CALL MSGPUT ('Will try for exact fit', 'I')
         TCHISQ = 0.0
      ELSE
         CALL MSGPUT ('Will try to fit to 1.0 sigma per point', 'I')
         DO 800 I = 0,NAXIS(1)-1
            IF(MEMR(WTADD+I).GT.0.0) THEN
               MEMX(SIGADD+I) = CMPLX(SIGMA,SIGMA)/SQRT(MEMR(WTADD+I))
            ELSE
               MEMX(SIGADD+I) = CMPLX(0.0, 0.0)
            END IF
            MEMR(WTADD+I)=1.0
 800     CONTINUE
         TCHISQ = VISSZ
      ENDIF
C
C Get default image
C
      IF (FILEXIST(DEFFILE)) THEN
         CALL FILIMGGE ('Default', DEFFILE, ' ')
         CALL MSGPUT ('Using specified image as default', 'I')
      ELSE
         CALL MSGPUT ('Using flat default', 'I')
         CALL IMGCLONE ('Util', 'Default')
         IF(FLUX.GT.0.0) THEN
            CALL ARRSETCO ('Default', FLUX, 0.0)
         ELSE
            CALL ARRSETCO ('Default', -FLUX, 0.0)
         END IF            
      END IF
C
C Get previously existing final image
C
      IF (.FALSE..AND.FILEXIST (WDFILE)) THEN
         CALL FILIMGGE ('WD', WDFILE, ' ')
         CALL ARRCLIP ('WD', 1E-8, 1E20, 'WD')
         IF (DATEXIST('WD/NITER')) THEN
            CALL DATGETI ('WD', 'NITER', BITER, 1, NDUMMY)
         ELSE
            BITER = 1
         END IF
         WRITE (MESSAGE, 1200) BITER
 1200    FORMAT ('Restarting WD from iteration ',I4)
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         BITER = 1
         CALL IMGCLONE ('Util', 'WD')
         CALL ARRCOPY ('Default', 'WD')
      END IF
C
C Set target flux
C
      IF (FLUX.GT.0.0) THEN
         TFLUX = FLUX
         CALL MSGPUT ('Total flux will be constrained exactly', 'I')
      ELSE
         TFLUX = 0.0
         CALL MSGPUT ('Total flux will not be constrained', 'I')
      END IF
C
C Find number of pixels in image
C
      CALL DATGETAR ('WD', NAX, NAXIS, ATYPE, WTADD)
      IMSZ = 1
      DO 810 I = 1, NAX
         IMSZ = IMSZ * NAXIS(I)
 810  CONTINUE
      IF(ERROR) GOTO 999
C
C
C ************************ Perform iterations ***********************
C
      CALL MSGPUT (
     $   ' Iteration         Potential      Fit    Gradient       Flux',
     $   'I')
      DO 200 ITER = BITER, NITER
         IF(ERROR) GOTO 999
         CALL MWDONEIT (ITER.EQ.BITER, VISSZ, IMSZ, NCONJ, TFLUX,
     $      MEMR(DATADD('WD')),
     $      MEMR(DATADD('Default')),
     $      MEMX(DATADD('Vis/Sigma/I/VIS')),
     $      MEMX(DATADD('Vis/OBS/I/VIS')),
     $      MEMX(DATADD('Vis/MOD/I/VIS')),
     $      MEMX(DATADD('Vis/Work1/I/VIS')),
     $      MEMX(DATADD('Vis/Work2/I/VIS')),
     $      MEMX(DATADD('Vis/Work3/I/VIS')),
     $      TCHISQ, TOL, LENGTH,
     $      MEMX(DATADD('Vis/Lambda/I/VIS')),
     $      MU, Z, FTRANS, LTRANS)
         CALL CCHISQ (CHISQ)
         IF (ERROR) GO TO 999
         CALL ARRSTAT ('WD', ' ')
         NOISE = SQRT(CHISQ/VISSZ)
         WRITE (MESSAGE, 1100) ITER, Z, NOISE, LENGTH,
     $      DATFGETR('WD','ARRSUM')
 1100    FORMAT (1X,I5,5X,F16.3,1X,3(F10.3,1X))
         CALL MSGPUT (MESSAGE, 'I')
C
C Write TV file if required
C
         IF((TVFILE.EQ.'WD')) THEN
            CALL IMGTVD (TVFILE, 0.0, 0.0)
         END IF
C
C Look to see if there were any interrupts: if there were then change
C some of the inputs
C
         IF (SYSINTRP) THEN
            SYSINTRP = .FALSE.
            IF (SYSINTAC.EQ.'QUIT') THEN
               FINISH = T
            ELSE IF (SYSINTAC.EQ.'INPUTS') THEN
               CALL USRGETI ('Niter', NITER, 1, NDUMMY)
               CALL USRGETC ('TV', TVFILE, 1, NDUMMY)
            END IF
         END IF
         IF(FINISH) GO TO 210
C
  200 CONTINUE
  210 CONTINUE
C
C ************************ End of Iterations ***********************
C
C Write answer
C
      CALL HISINPUT ('WD')
      CALL DATPUTI ('WD', 'NITER', ITER, 1)
      CALL HISOPEN ('WD')
      CALL HISPUT ('WD', 
     $  ' Iteration         Potential      Fit    Gradient       Flux'
     $   )
      CALL ARRSTAT ('WD', ' ')
      WRITE (MESSAGE, 1100) ITER, Z, NOISE, LENGTH,
     $      DATFGETR('WD','ARRSUM')
      CALL HISPUT ('WD', MESSAGE)
C
C Now write output images as required
C
      CALL USRGETC ('WD', WDFILE, 1, NDUMMY)
      IF (WDFILE.NE.' ' ) THEN         
         CALL DATPUTC ('WD', 'BUNIT', 'JY/PIXEL', 1)
         CALL FILIMGPU ('WD', WDFILE, ' ')
      END IF
C
      CALL USRGETC ('CWD', CWDFILE, 1, NDUMMY)
      IF (CWDFILE.NE.' ') THEN
         CALL IMGDOUBL ('WD', 'DUtil')
         CALL IMGGRIDC ('DUtil', 'DUtil', 'CORRECT')
         CALL IMGFFT ('DUtil', 'DWDVis')
         CALL VISDEGRI ('Vis', 'MOD/I', 'DWDVis')
         CALL ARRSUBTR ('Vis/OBS/I/VIS', 'Vis/MOD/I/VIS',
     $      'Vis/MOD/I/VIS')
         CALL IMGSMOOT ('WD', BEAM, 'WD', 'WDVis')
         CALL IMGP2PB ('WD', BEAM, 'WD')
         CALL DATPUTC ('WD', 'BUNIT', 'JY/BEAM', 1)
         CALL ARRADD ('WD', 'Util', 'WD')
         CALL FILIMGPU ('WD', CWDFILE, ' ')
      END IF
C
C
 999  CONTINUE
      END
      SUBROUTINE LTRANS (IMSZ, DATASZ, LAMBDA, LAMTRNS)
C 
C Calculate Transform of Lagrange multipliers: note that the
C arguments are not used.
C
C Audit trail:
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	LAMBDA(*), LAMTRNS(*)
      INTEGER	IMSZ, DATASZ
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'LTRANS')
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL VISTOIMG ('Vis', 'Lambda/I', 'WD', 'WDVis', .FALSE.)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
      SUBROUTINE FTRANS (IMSZ, DATASZ, IMG, IMGTRNS)
C 
C Calculate Transform of Image
C
C Audit trail:
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	IMG, IMGTRNS
      INTEGER	IMSZ, DATASZ
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTRANS')
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL IMGDOUBL ('WD', 'DUtil')
      CALL IMGGRIDC ('DUtil', 'DUtil', 'CORRECT')
      CALL IMGFFT ('DUtil', 'DWDVis')
      CALL VISDEGRI ('Vis', 'MOD/I', 'DWDVis')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
      SUBROUTINE CCHISQ (CHISQ)
C 
C Calculate chi-squared of fit
C
C	CHISQ	REAL	output	Chi-squared
C
C Audit trail:
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		CHISQ
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CCHISQ')
C
      INTEGER		I, N1, NAXIS1(SYSMXDIM)
      CHARACTER*1	T1
      INTEGER		ADD1, ADD2, ADD3
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR ('Vis/OBS/I/VIS', N1, NAXIS1, T1, ADD1)
      CALL DATGETAR ('Vis/MOD/I/VIS', N1, NAXIS1, T1, ADD2)
      CALL DATGETAR ('Vis/Sigma/I/VIS', N1, NAXIS1, T1, ADD3)
      IF(ERROR) GO TO 990
C
      CHISQ = 0.0
      DO 110 I = 0, NAXIS1(1)-1
         IF(REAL(MEMX(ADD3+I)).GT.0.0) THEN
            CHISQ = CHISQ + ABS(MEMX(ADD1+I)-MEMX(ADD2+I))**2 /
     $         REAL(MEMX(ADD3+I))**2
         END IF
 110  CONTINUE
C
      IF (CHISQ.LT.0.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Chisquared < 0.0')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
