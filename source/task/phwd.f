C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C  @(#)phwd.f	1.1 5/6/92
C
      SUBROUTINE SDEMAIN
C
CD Program to perform maximum entropy deconvolution.
C Uses the Wilczek-Drapatz algorithm modified for conjugate gradients 
C in place of Newton Raphson.
C
C Audit trail:
C	New task: solves exactly the same problem as phvm
C					T.J. Cornwell 6 May 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PHWD')
C
      INTEGER		NITER, BITER, ITER, NDUMMY, IMSZ, IAX, NAX, 
     1			NAXIS(SYSMXDIM), NCONJ, DATADD
      REAL		TCHISQ, CHISQ, Z, TOL, LENGTH, SCALE, 
     2			READOUT, DATFGETR, MU, NOISE, TFLUX
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	WDFILE, DEFFILE, DRTFILE, PSFFILE, 
     1			TVFILE
      LOGICAL		DATEXIST, FILEXIST
      EXTERNAL		FTRANS, LTRANS
C==================================================================
      CALL MSGWELCO (
     &   'I perform photon-limited maximum entropy deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Default', DEFFILE, 1, NDUMMY)
      CALL USRGETC ('WD', WDFILE, 1, NDUMMY)
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETI ('Nconj', NCONJ, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETR ('Readout', READOUT, 1, NDUMMY)
      CALL USRGETR ('Scale', SCALE, 1, NDUMMY)
      CALL USRGETR ('Tolerance', TOL, 1, NDUMMY)
      CALL USRGETC ('TV', TVFILE, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Get images
C
      CALL FILIMGGE ('DObs', DRTFILE, ' ')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
C
C Rescale to Photons and Set target flux
C
      CALL ARRSCALE ('DObs', SCALE, 0.0, 'DObs')
      CALL ARRSTAT ('DObs', ' ')
C
      CALL DATGETR ('DObs', 'ARRSUM', TFLUX, 1, NDUMMY)
      WRITE(MESSAGE,3000) TFLUX
 3000 FORMAT ('Total number of photons = ',F12.1)
      CALL MSGPUT (MESSAGE, 'I')
C
      IF (FILEXIST(DEFFILE)) THEN
         CALL FILIMGGE ('Default', DEFFILE, ' ')
         CALL MSGPUT ('Using specified image as default', 'I')
      ELSE
         CALL MSGPUT ('Using flat default', 'I')
         CALL IMGCLONE ('DObs', 'Default')
         CALL ARRSETCO ('Default', TFLUX, 0.0)
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
C
C Make the PSF
C
      CALL IMGMAKEX ('PSF', 'XFR')
C
C Clone some images
C
      CALL IMGCLONE ('DObs', 'Lambda')
      CALL IMGCLONE ('DObs', 'DPred')
      CALL IMGCLONE ('DObs', 'Sigma')
      CALL IMGCLONE ('DObs', 'Work1')
      CALL IMGCLONE ('DObs', 'Work2')
      CALL IMGCLONE ('DObs', 'Work3')
      CALL IMGCLONE ('XFR', 'WDFT')
      CALL ARRCOPY ('DObs', 'DPred')
C
C Find image size and set desired Chi-squared
C
      CALL DATGETAR ('WD', NAX, NAXIS, ATYPE, NDUMMY)
      IF (ERROR) GO TO 999
      IMSZ = 1
      DO 10 IAX = 1, NAX
         IMSZ = IMSZ * MAX(1, NAXIS(IAX))
  10  CONTINUE
      TCHISQ=IMSZ
C
C ************************ Perform iterations ***********************
C
      CALL MSGPUT (
     $   ' Iteration         Potential      Fit     Gradient  Photons',
     $   'I')
      ITER=BITER
 200  CONTINUE
         CALL ARRSCALE ('DPred', 1.0, READOUT**2, 'Sigma')
         CALL ARRSQRT ('Sigma', 'Sigma')
         CALL MWDONEIT (ITER.EQ.BITER, IMSZ, IMSZ, NCONJ, 0.0,
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
 1100    FORMAT (1X,I5,5X,F16.3,1X,2(F10.3,1X),F13.0)
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
               GO TO 210
            ELSE IF (SYSINTAC.EQ.'INPUTS') THEN
               CALL USRGETI ('Niter', NITER, 1, NDUMMY)
               CALL USRGETC ('TV', TVFILE, 1, NDUMMY)
            END IF
         END IF
         ITER=ITER+1
         IF(ITER.LT.NITER) GO TO 200
C
  210 CONTINUE
C
C ************************ End of Iteration ************************
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
C
      IF (WDFILE.NE.' ' ) THEN         
         CALL FILIMGPU ('WD', WDFILE, ' ')
      END IF
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
      CALL IMGFFT ('Lambda', 'WDFT')
      CALL ARRCMULT ('WDFT', 'XFR', 'WDFT')
      CALL IMGFFT ('WDFT', 'WD')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
      SUBROUTINE FTRANS (IMSZ, DATASZ, IMG, IMGTRNS)
C 
C Calculate Predicted Dirty Image from current estimate
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
      CALL IMGFFT ('WD', 'WDFT')
      CALL ARRMULT ('WDFT', 'XFR', 'WDFT')
      CALL IMGFFT ('WDFT', 'DPred')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
      SUBROUTINE CCHISQ (CHISQ)
C 
C Calculate chi-squared of fit for photon limited data
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
      IF(ERROR) GO TO 990
C
      NT = 1
      DO 10 I = 1, N1
         NT = NT * NAXIS1(I)
  10  CONTINUE 
C
      CHISQ = 0.0
      DO 110 I = 0, NT-1
         IF(MEMR(ADD3+I).GT.1E-3) THEN
            CHISQ = CHISQ + (MEMR(ADD1+I)-MEMR(ADD2+I))**2 
     $         / MEMR(ADD3+I)**2
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
