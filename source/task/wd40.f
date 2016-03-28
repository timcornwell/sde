C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C   @(#)wd40.f	1.4   6/15/91
C
      SUBROUTINE SDEMAIN
C
CD Program to perform maximum entropy deconvolution.
C Uses the Wilczek-Drapatz algorithm modified for conjugate gradients 
C in place of Newton Raphson.
C
C Audit trail:
C	Allow flux to float
C					T.J. Cornwell 15 June 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'WD40')
C
      INTEGER		NITER, BITER, ITER, NDUMMY, IMSZ, IAX, NAX, 
     1			NAXIS(SYSMXDIM), NCONJ, DATADD
      REAL		FLUX, TCHISQ, CHISQ, Z, TOL, LENGTH, SIGMA,
     2			BEAM(4), DATFGETR, MU, NOISE, TFLUX
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	WDFILE, DEFFILE, DRTFILE, PSFFILE, 
     1			CWDFILE, SIGFILE, TVFILE
      LOGICAL		DATEXIST, FILEXIST
      LOGICAL		FINISH, T, F
      EXTERNAL		FTRANS, LTRANS
      DATA		T	/.TRUE./
      DATA		F	/.FALSE./
C==================================================================
      CALL MSGWELCO ('I perform maximum entropy deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Default', DEFFILE, 1, NDUMMY)
      CALL USRGETC ('Error', SIGFILE, 1, NDUMMY)
      CALL USRGETC ('WD', WDFILE, 1, NDUMMY)
      CALL USRGETC ('CWD', CWDFILE, 1, NDUMMY)
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETI ('Nconj', NCONJ, 1, NDUMMY)
      CALL USRGETR ('Tflux', FLUX, 1, NDUMMY)
      CALL USRGETR ('Sigma', SIGMA, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETR ('Tolerance', TOL, 1, NDUMMY)
      CALL USRGETC ('TV', TVFILE, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Get images
C
      CALL FILIMGGE ('DObs', DRTFILE, ' ')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
      IF (FILEXIST(DEFFILE)) THEN
         CALL FILIMGGE ('Default', DEFFILE, ' ')
         CALL MSGPUT ('Using specified image as default', 'I')
      ELSE
         CALL MSGPUT ('Using flat default', 'I')
         CALL IMGCLONE ('DObs', 'Default')
         IF(FLUX.GT.0.0) THEN
            CALL ARRSETCO ('Default', FLUX, 0.0)
         ELSE
            CALL ARRSETCO ('Default', -FLUX, 0.0)
         END IF            
      END IF
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
         CALL IMGCLONE ('DObs', 'WD')
         CALL ARRCOPY ('Default', 'WD')
      END IF
      IF(FILEXIST (SIGFILE)) THEN
         CALL FILIMGGE ('Error', SIGFILE, ' ')
      ELSE
         CALL IMGCLONE ('DObs', 'Sigma')
         IF (SIGMA.EQ.0.0) THEN
            CALL MSGPUT ('Units of fit will those of dirty image', 'I')
            CALL ARRSETCO ('Sigma', 0.0, 1.0)
         ELSE
            CALL ARRSETCO ('Sigma', 0.0, SIGMA)
         END IF
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
C Fit a Gaussian Beam to PSF
C
      IF (BEAM(1).EQ.0.0) THEN
         CALL IMGBMSHP('PSF')
         IF (ERROR) GO TO 999
         BEAM(1) = DATFGETR('PSF','BMAJ')*3600.0
         BEAM(2) = DATFGETR('PSF','BMIN')*3600.0
         BEAM(3) = DATFGETR('PSF','BPA')
         BEAM(4) = DATFGETR('PSF','BZ')*3600.0
      ENDIF
C
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL IMGCLONE ('XFR', 'WDVis')
C
C Clone some images
C
      CALL IMGCLONE ('DObs', 'Lambda')
      CALL IMGCLONE ('DObs', 'DPred')
C
C Find image size
C
      CALL DATGETAR ('WD', NAX, NAXIS, ATYPE, NDUMMY)
      IF (ERROR) GO TO 999
      IMSZ = 1
      DO 10 IAX = 1, NAX
         IMSZ = IMSZ * MAX(1, NAXIS(IAX))
  10  CONTINUE
      IF(SIGMA.EQ.0.0) THEN
         CALL MSGPUT ('Will try for exact fit', 'I')
         TCHISQ = 0.0
      ELSE
         CALL MSGPUT ('Will try to fit to 1.0 sigma per point', 'I')
         TCHISQ = IMSZ
      ENDIF
      CALL IMGCLONE ('DObs', 'Work1')
      CALL IMGCLONE ('DObs', 'Work2')
      CALL IMGCLONE ('DObs', 'Work3')
C
C ************************ Perform iterations ***********************
C
      CALL MSGPUT (
     $   ' Iteration         Potential      Fit    Gradient       Flux',
     $   'I')
      DO 200 ITER = BITER, NITER
         CALL MWDONEIT (ITER.EQ.BITER, IMSZ, IMSZ, NCONJ, TFLUX,
     $      MEMR(DATADD('WD')), MEMR(DATADD('Default')),
     $      MEMR(DATADD('Sigma')), MEMR(DATADD('DObs')),
     $      MEMR(DATADD('DPred')), MEMR(DATADD('Work1')),
     $      MEMR(DATADD('Work2')), MEMR(DATADD('Work3')),
     $      TCHISQ, TOL, LENGTH, MEMR(DATADD('Lambda')), MU, Z,
     $      FTRANS, LTRANS)
         CALL CCHISQ (CHISQ)
         IF (ERROR) GO TO 999
         CALL ARRSTAT ('WD', ' ')
         NOISE = SQRT(CHISQ/IMSZ)
         WRITE (MESSAGE, 1100) ITER, Z, NOISE, LENGTH,
     $      DATFGETR('WD','ARRSUM')
 1100    FORMAT (1X,I5,5X,F16.3,1X,3(F10.3,1X))
         CALL MSGPUT (MESSAGE, 'I')
C
C Write TV file if required
C
         IF((TVFILE.EQ.'WD').OR.(TVFILE.EQ.'Lambda').OR.
     $      (TVFILE.EQ.'Work1').OR.(TVFILE.EQ.'Work2').OR.
     $      (TVFILE.EQ.'Work3')) THEN
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
C ************************ End of Iteration ************************
C
C Housekeeping
C
      CALL DATDELET ('Default')
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
      CALL USRGETC ('CWD', CWDFILE, 1, NDUMMY)
C
      IF (WDFILE.NE.' ' ) THEN         
         CALL DATPUTC ('WD', 'BUNIT', 'JY/PIXEL', 1)
         CALL FILIMGPU ('WD', WDFILE, ' ')
      END IF
C
      IF (CWDFILE.NE.' ') THEN
         CALL IMGRESID ('WD', 'DObs', 'XFR', 'DPred', 'WDVis')
         CALL IMGSMOOT ('WD', BEAM, 'WD', 'WDVis')
         CALL ARRADD ('WD', 'DPred', 'WD')
         IF (TVFILE.EQ.'WD') THEN
            CALL IMGTVD (TVFILE, 0.0, 0.0)
         END IF
         CALL FILIMGPU ('WD', CWDFILE, ' ')
      END IF
C
 999  CONTINUE
      END
      SUBROUTINE LTRANS (IMSZ, DATASZ, LAMBDA, LAMTRNS)
C 
C Calculate Transform of Lagrange multipliers: note that the
C arguments are not used.
C
C	IMSZ	INTEGER	input	Number of elements in LAMTRNS
C	DATASZ	INTEGER	input	Number of elements in LAMBDA
C	LAMBDA	REAL	input	Image of lambdas
C	LAMTRNS	REAL	input	Image of transformed lambdas
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
      CALL IMGFFT ('Lambda', 'WDVis')
      CALL ARRCMULT ('WDVis', 'XFR', 'WDVis')
      CALL IMGFFT ('WDVis', 'WD')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
      SUBROUTINE FTRANS (IMSZ, DATASZ, IMG, IMGTRNS)
C 
C Calculate Transform of Image
C
C	IMSZ	INTEGER	input	Number of elements in IMG
C	DATASZ	INTEGER	input	Number of elements in IMGTRNS
C	IMG	REAL	input	Image
C	IMGTRNS	REAL	input	Name of transformed image
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
      CALL IMGFFT ('WD', 'WDVis')
      CALL ARRMULT ('WDVis', 'XFR', 'WDVis')
      CALL IMGFFT ('WDVis', 'DPred')
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
      INTEGER		I, N1, NAXIS1(SYSMXDIM), DATADD
      CHARACTER*1	T1
      INTEGER		ADD1, ADD2, ADD3, NT
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR ('DObs', N1, NAXIS1, T1, ADD1)
      ADD2 = DATADD ('DPred')
      ADD3 = DATADD ('Sigma')
      NT = 1
      DO 10 I = 1, N1
         NT = NT * NAXIS1(I)
  10  CONTINUE 
      IF(ERROR) GO TO 990
C
      CHISQ = 0.0
      DO 110 I = 0, NT-1
         IF(MEMR(ADD3+I).GT.0.0) THEN
            CHISQ = CHISQ + (MEMR(ADD1+I)-MEMR(ADD2+I))**2 /
     $         MEMR(ADD3+I)**2
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
