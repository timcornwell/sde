C
C       National Radio Astronomy Observatory, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)phvm.f	1.5    5/12/92
C
      SUBROUTINE SDEMAIN
C
#define maximg 32
C
CD Program to perform maximum entropy or emptiness deconvolution for 
C photon-limited images
C
C Audit trail:
C       Added scaling and readout noise
C                                       T.J. Cornwell 25 July 1990
C       Added emptiness
C                                       T.J. Cornwell 9 August 1990
C       Fixed Restart problem
C                                       T.J. Cornwell 6 May 1992
C       Put in trap for gradient getting too small
C                                       R.S. Simon 12 May 1992
C-----------------------------------------------------------------------
#include        "stdinc.h"
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'PHVM')
C
      INTEGER           NITER, BITER, ITER, NDUMMY, IMSZ, IAX, NAX, 
     1                  NAXIS(SYSMXDIM), AADD, VMLIM
      REAL              DEFLEV, TFLUX, FLUX, ITFLUX, ENTRPY, TOL, 
     1                  NRMGRD, Q, TOLER, NOISE, SCALE, READOUT,
     2                  MAXNMGRD, CHISQ, TCHISQ, AFIT
      REAL              PRVGRD
      CHARACTER*1       ATYPE
      CHARACTER*(SYSMXNAM)      VMFILE, DEFFILE, DRTFILE(maximg), 
     1                  PSFFILE(maximg), TVFILE, ENTTYPE
      LOGICAL           DATEXIST, FILEXIST
      LOGICAL           DOSTOP, FINISH, CNVRGE, T, F
      EXTERNAL          CALGCH
      CHARACTER*6       STRINT
      INTEGER           IMG
      INTEGER           NIMG
      COMMON            /PHVMCOM/ NIMG, READOUT
      DATA              T       /.TRUE./
      DATA              F       /.FALSE./
C==================================================================
      CALL MSGWELCO ('I perform maximum entropy deconvolution'//
     $   ' for photon-limited images')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, maximg, NIMG)
      CALL USRGETC ('PSF', PSFFILE, maximg, NIMG)
      CALL USRGETC ('Default', DEFFILE, 1, NDUMMY)
      CALL USRGETC ('VM', VMFILE, 1, NDUMMY)
      CALL USRGETR ('Scale', SCALE, 1, NDUMMY)
      CALL USRGETR ('Readout', READOUT, 1, NDUMMY)
      READOUT = SCALE * READOUT
      CALL USRGETC ('Entropy', ENTTYPE, 1, NDUMMY)
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETR ('Tolerance', TOLER, 1, NDUMMY)
      CALL USRGETR ('Npoints', Q, 1, NDUMMY)
      CALL USRGETR ('Gain', TOL, 1, NDUMMY)
      CALL USRGETR ('MaxGrad', MAXNMGRD, 1, NDUMMY)
      CALL USRGETC ('TV', TVFILE, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
      VMLIM = -30
      DOSTOP = NITER.LT.0.0
      IF (NITER.NE.0) VMLIM = ABS(NITER)
C
C Find file names
C
      DO 5 IMG = 1, maximg
         IF ((DRTFILE(IMG).EQ.' ').OR.(PSFFILE(IMG).EQ.' ')) THEN
            NIMG = IMG - 1
            GO TO 6
         END IF
 5    CONTINUE
 6    CONTINUE
C
C Get images
C
      TFLUX = 0.0
      DO 10 IMG = 1, NIMG
         CALL FILIMGGE ('Dirty'//STRINT(IMG), DRTFILE(IMG), ' ')
         CALL ARRSCALE ('Dirty'//STRINT(IMG), SCALE, 0.0,
     $      'Dirty'//STRINT(IMG))
         CALL ARRCLIP ('Dirty'//STRINT(IMG), 0.0, 1E20,
     $      'Dirty'//STRINT(IMG))
         CALL FILIMGGE ('PSF'//STRINT(IMG), PSFFILE(IMG), ' ')
         CALL ARRCLIP ('PSF'//STRINT(IMG), 0.0, 1E20,
     $      'PSF'//STRINT(IMG))
C
C Find total flux: assume that PSF has unit volume
C
         CALL ARRSTAT ('Dirty'//STRINT(IMG), ' ')
         CALL DATGETR ('Dirty'//STRINT(IMG), 'ARRSUM', ITFLUX, 1,
     $      NDUMMY)
         TFLUX = TFLUX + ITFLUX
C
C Now make transfer functions, etc. 
C
         CALL IMGMAKEX ('PSF'//STRINT(IMG), 'XFR'//STRINT(IMG))
         CALL DATDELET ('PSF'//STRINT(IMG))
 10   CONTINUE
C
C Find image size
C
      CALL DATGETAR ('Dirty1', NAX, NAXIS, ATYPE, AADD)
      IF (ERROR) GO TO 999
      IMSZ = 1
      DO 11 IAX = 1, NAX
         IMSZ = IMSZ * MAX(1, NAXIS(IAX))
 11   CONTINUE
C
      TFLUX = TFLUX / FLOAT(NIMG)
      WRITE(MESSAGE,3000) TFLUX
 3000 FORMAT ('Total number of photons = ',F12.1)
      CALL MSGPUT (MESSAGE, 'I')
C
      CALL IMGCLONE ('XFR1', 'VMVis')
C
      IF (FILEXIST (VMFILE)) THEN
         CALL FILIMGGE ('VM', VMFILE, ' ')
         CALL ARRSCALE ('VM', SCALE, 0.0, 'VM')
         IF(ENTTYPE(1:1).EQ.'H') THEN
            CALL ARRCLIP ('VM', 1E-8, 1E20, 'VM')
         END IF
         CALL DATGETI ('VM', 'NITER', BITER, 1, NDUMMY)
         IF (ERROR) THEN
            CALL ERRCANCE
            BITER = 0
         END IF
         WRITE (MESSAGE, 1200) BITER
 1200    FORMAT ('Restarting VM from iteration ',I4)
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         BITER = 0
         CALL IMGCLONE ('Dirty1', 'VM')
         CALL ARRSETCO ('VM', ABS(TFLUX), 0.0)
      END IF
C
C Clone some images
C
      CALL IMGCLONE ('Dirty1', 'PDirty')
      CALL IMGCLONE ('Dirty1', 'Step')
      CALL IMGCLONE ('Dirty1', 'Util')
      CALL IMGCLONE ('Dirty1', 'IWeight')
      CALL IMGCLONE ('Dirty1', 'GradCh')
      CALL IMGCLONE ('Dirty1', 'Weight')
C
C Now take care of default image
C
      IF (FILEXIST(DEFFILE)) THEN
         CALL FILIMGGE ('Default', DEFFILE, ' ')
         CALL MSGPUT ('Using specified image as default', 'I')
         CALL ARRSCALE ('Default', SCALE, 0.0, 'Default')
         IF(ENTTYPE(1:1).EQ.'H') THEN
            DEFLEV = 0.0
            CALL ARRCLIP ('Default', 1E-8, 1E20, 'Default')
         ELSE
            DEFLEV=-2E20
         END IF
         CALL ARRCOPY ('Default', 'VM')
      ELSE
         IF(ENTTYPE(1:1).EQ.'H') THEN
            DEFLEV = ABS(TFLUX)/IMSZ
            CALL MSGPUT ('Using flat default', 'I')
         ELSE
            DEFLEV = 0.0
            CALL MSGPUT ('Using zero default', 'I')
         END IF
      END IF
C
C ************************ Perform iterations ***********************
C
      CALL CALGCH ('VM', 'GradCh', CHISQ)
      ITER = BITER 
      IF(ENTTYPE(1:1).EQ.'H') THEN
         CALL MSGPUT (
     1'Iteration            Entropy      Photons     Fit     Gradient',
     2      'I')
      ELSE
         CALL MSGPUT (
     1'Iteration   Photons        Fit     Gradient',
     2      'I')
      END IF
      FINISH = F
      CNVRGE = F
      TCHISQ = IMSZ * NIMG
  200 CONTINUE
      IF(ENTTYPE(1:1).EQ.'H') THEN
         CALL MEWONEIT (ITER.EQ.BITER, 'VM', 'Default', DEFLEV, 'Step',
     $      'GradCh', 'Weight', Q, TFLUX, TCHISQ, TOL, FLUX, CHISQ, 
     $      ENTRPY, NRMGRD, CALGCH)
         NOISE = SQRT(ABS(CHISQ/TCHISQ))
         ITER = ITER + 1
         WRITE (MESSAGE, 1100) ITER, ENTRPY, FLUX, NOISE, NRMGRD
 1100    FORMAT (1X,I4,2X,F16.3,1X,1(F14.1,1X),2(F11.4,1X))
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         AFIT = SQRT(ABS(CHISQ/TCHISQ))
         CALL MUWONEIT (ITER.EQ.BITER, 'VM', 'Default', DEFLEV, 'Step',
     $      'GradCh', 'Weight', Q, TFLUX, TCHISQ, TOL, FLUX, CHISQ, 
     $      ENTRPY, NRMGRD, AFIT, CALGCH)
         NOISE = SQRT(ABS(CHISQ/TCHISQ))
         ITER = ITER + 1
         WRITE (MESSAGE, 1101) ITER, FLUX, NOISE, NRMGRD
 1101    FORMAT (1X,I5,5X,3(F10.3,1X))
         CALL MSGPUT (MESSAGE, 'I')
      END IF
      IF (ITER.GE.VMLIM) FINISH = T
      IF (ERROR) GO TO 999
C
C Write TV file if required
C
      IF((TVFILE.EQ.'VM').OR.(TVFILE.EQ.'Step').OR.
     $   (TVFILE.EQ.'GradCh')) THEN
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
            DOSTOP = NITER.LT.0.0
            IF (NITER.NE.0) VMLIM = IABS(NITER)
            CALL USRGETC ('TV', TVFILE, 1, NDUMMY)
         END IF
      END IF
C
C Check if it's time to stop
C
      CNVRGE = (NOISE.LT.1.0+TOLER) .AND. 
     1   (ITER.NE.(BITER+1)) .AND. (NRMGRD.LT.TOLER)
      IF (TFLUX.GT.0.0) THEN
         CNVRGE =  CNVRGE .AND. (ABS(FLUX-TFLUX).LT.TOLER*TFLUX)
      END IF
      IF (CNVRGE) THEN
         IF (DOSTOP) THEN
            FINISH = T
         ELSE 
            TOL = NRMGRD
         END IF
      END IF
      IF ((NRMGRD.GT.MAXNMGRD).AND.(ITER.GT.1)) THEN
         CALL MSGPUT ('Excessive gradient: stopping now', 'I')
         FINISH = T
      END IF
      IF ((NRMGRD.LT.0.0001).AND.(PRVGRD.LT.0.0001).AND.
     1    (ITER.GT.2)) THEN
         CALL MSGPUT ('Excessively small gradient: stopping now', 'I')
         FINISH = T
      END IF
      PRVGRD = NRMGRD
      IF (.NOT.FINISH) GO TO 200
C
C ************************ End of Iteration ************************
C
C Housekeeping
C
      IF (DATEXIST('Default')) THEN
         CALL DATDELET ('Default')
      END IF
      CALL DATDELET ('Step')
C
C Write answer
C
      CALL HISINPUT ('VM')
      CALL DATPUTI ('VM', 'NITER', ITER, 1)
      CALL HISOPEN ('VM')
      IF(ENTTYPE(1:).EQ.'H') THEN
         CALL HISPUT ('VM', 
     1   'Iteration        Entropy    Photons      Fit     Gradient')
         WRITE (MESSAGE, 1100) ITER, ENTRPY, FLUX, NOISE, NRMGRD
      ELSE
         CALL HISPUT ('VM', 
     1   'Iteration  Photons     Fit     Gradient')
         WRITE (MESSAGE, 1101) ITER, FLUX, NOISE, NRMGRD
      END IF
      CALL HISPUT ('VM', MESSAGE)
C
C Convert back to original units
C
      CALL ARRSCALE ('VM', 1.0/SCALE, 0.0, 'VM')
C
C Now write output image
C
      CALL USRGETC ('VM', VMFILE, 1, NDUMMY)
C
      IF (VMFILE.NE.' ' ) THEN         
         CALL DATPUTC ('VM', 'BUNIT', 'PH/PIXEL', 1)
         CALL FILIMGPU ('VM', VMFILE, ' ')
      END IF
C
 999  CONTINUE
      END
C++
      SUBROUTINE CALGCH (MODEL, GCH, CHISQ)
C
C Calculate chi-squared and grad chi-squared.
C
C       MODEL   CH*(*)  input   Name of model image
C       GCH     CH*(*)  input   Name of grad chi-squared
C Audit trail:
C       Original version: Audit trail comments go on this line
C       and successive lines
C                               T.J.Cornwell    Jan 5 1989
C
C-----------------------------------------------------------------------
#include        "stdinc.h"
C
      REAL              CHISQ
      CHARACTER*(*)     MODEL, GCH
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'CALGCH')
      REAL              ICHISQ, READOUT, SUMWT
      CHARACTER*6       STRINT
      INTEGER           IMG
      INTEGER           NIMG
      COMMON            /PHVMCOM/ NIMG, READOUT
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL ARRSETCO ('Weight', 0.0, 0.0)
      CALL ARRSETCO (GCH, 0.0, 0.0)
C
C Loop over all images
C
      CHISQ = 0.0
      DO 10 IMG = 1, NIMG
C
C Find predicted dirty image
C
         CALL IMGCONV (MODEL, ' ', 'XFR'//STRINT(IMG), 'PDirty',
     $      'VMVis')
C
C Add readout noise term to weight image before inverting. Also
C accumulate total weight
C
         CALL ARRSCALE ('PDirty', 1.0, READOUT**2, 'IWeight')
         CALL ARRCINV ('IWeight', 1E-3, 'IWeight')
         CALL ARRADD ('Weight', 'IWeight', 'Weight')
C
C Find chi-squared
C
         CALL ARRSUBTR ('Dirty'//STRINT(IMG), 'PDirty', 'Util')
         CALL ARRWCHIS ('Util', 'IWeight', ICHISQ, SUMWT)
         CHISQ = CHISQ + ICHISQ
C
C Now find grad chi-squared
C
         CALL ARRMULT ('Util', 'IWeight', 'Util')
         CALL IMGCORR ('Util', ' ', 'XFR'//STRINT(IMG), 'Util',
     $      'VMVis')
         CALL ARRLC (GCH, 1.0, 'Util', -2.0, GCH)
 10   CONTINUE
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END


