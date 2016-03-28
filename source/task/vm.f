C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vm.f	1.17    6/14/94
#define MINFLUX 1.0E-8
#define MAXFLUX 1.0E20
C
      SUBROUTINE SDEMAIN
C
CD Program to perform maximum entropy deconvolution. This version
C calculates residuals in the image-plane and approximates the
C true visibility-plane chi-squared by the image-plane misfit
C divided by the 'number of points per beam', q. For this to work
C we have to continually adjust the required chi-squared to remain
C in step with the changing value of q. IMGCLRES is the routine
C which calculates the residuals.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added beam fitting
C				T.J. Cornwell	Sept 13 1989
C	Added TV file
C				T.J. Cornwell	Sept 20 1989
C	If imageplane then allow deconvolution of whole window
C				T.J. Cornwell	Nov 20 1989
C	Added Emptiness
C				T.J. Cornwell	Feb 1 1990
C	Added ICFs as in MEMSYS
C				T.J. Cornwell   Nov 1 1991
C	Fixed restart problem
C				T.J. Cornwell   May 6 1992
C	Added soft windows, residual image, and auto contraction of
C	the window.  (Use cleanwin to remake images at full size)
C				D.S. Briggs	March 27 1993
C	Save and restore current estimate of diagonal element, Q,
C	across restarts
C				D.S.Briggs	May 8 1994
C	Added selction of BEAMFIT algorithm based on value of BEAM(1)
C				D.S.Briggs	May 19 1994
C	Tweaked BEAMFIT algotithm selection to use FITALG
C				D.S.Briggs	June 14 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VM')
C
      INTEGER		NITER, BITER, ITER, NDUMMY, IMSZ, IAX, NAX, 
     $			NAXIS(SYSMXDIM), AADD, VMLIM, 
     $			BLC (SYSMXDIM), TRC (SYSMXDIM),
     $   		BBBLC(SYSMXDIM), BBTRC(SYSMXDIM)
      REAL		DEFLEV, TFLUX, TCHISQ, FLUX, CHISQ, ENTRPY, TOL, 
     $			NRMGRD, Q, SIGMA, NSIGMA, NOISE, TOLER, AFIT,
     $			BEAM(4), MAXNMGRD, ICF(4)
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	VMFILE, DEFFILE, DRTFILE, PSFFILE, 
     $			CVMFILE, BUNIT, TVFILE, ENTTYPE, BOXFILE,
     $   		RESFILE, TMPSTR, FITALG
      LOGICAL		DOSTOP, FINISH, CNVRGE, T, F
      EXTERNAL		IMGCLRES
      LOGICAL		IMGPLANE, DOICF, DOSOFTW
      COMMON		/VMCOM/	IMGPLANE, DOICF, ICF, DOSOFTW
      DATA		T	/.TRUE./
      DATA		F	/.FALSE./
C
      REAL		DATFGETR
      LOGICAL		DATEXIST, FILEXIST
C==================================================================
      CALL MSGWELCO ('I perform maximum entropy deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Default', DEFFILE, 1, NDUMMY)
      CALL USRGETC ('VM', VMFILE, 1, NDUMMY)
      CALL USRGETC ('CVM', CVMFILE, 1, NDUMMY)
      CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
      CALL USRGETC ('Box', BOXFILE, 1, NDUMMY)
      IF (ERROR) GO TO 999
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      VMLIM = 5
      DOSTOP = NITER.LT.0.0
      IF (NITER.NE.0) VMLIM = IABS(NITER)
      CALL USRGETR ('Tflux', TFLUX, 1, NDUMMY)
      CALL USRGETR ('Sigma', SIGMA, 1, NDUMMY)
      CALL USRGETC ('Entropy', ENTTYPE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETR ('ICF', ICF, 4, NDUMMY)
      CALL USRGETR ('Tolerance', TOLER, 1, NDUMMY)
      CALL USRGETR ('Npoints', Q, 1, NDUMMY)
      CALL USRGETR ('Gain', TOL, 1, NDUMMY)
      CALL USRGETR ('MaxGrad', MAXNMGRD, 1, NDUMMY)
      CALL USRGETL ('ImagePlane', IMGPLANE, 1, NDUMMY)
      CALL USRGETC ('TV', TVFILE, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
      IF (ENTTYPE(1:1).EQ.'H') THEN
         CALL MSGPUT ('Maximising entropy -I*logI', 'I')
      ELSE
         CALL MSGPUT ('Maximising emptiness', 'I')
      END IF
C
      IF (SIGMA.LE.0.0) THEN
         SIGMA = 0.01
      END IF
C
C Get images
C
      CALL FILIMGGE ('InitialDirty', DRTFILE, ' ')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
      DOSOFTW = .FALSE.
      IF (BOXFILE.NE.' ') THEN
         DOSOFTW = .TRUE.
         CALL FILMASGE ('Box', BOXFILE, 'InitialDirty')
      END IF
C
C Contract Window if requested
C
      IF ((BLC(1).LT.0).OR.(TRC(1).LT.0)) THEN
         IF (.NOT.DOSOFTW) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Must use BOX adverb for auto-contraction')
            GO TO 999
         END IF
C
         IF (.NOT.DATEXIST('Box/ARRMAX')) CALL ARRSTAT('Box', ' ')
         CALL DATGETI ('Box', 'ARRBBBLC', BBBLC, SYSMXDIM, NDUMMY)
         CALL DATGETI ('Box', 'ARRBBTRC', BBTRC, SYSMXDIM, NDUMMY)
         CALL CRDSHBOX ('Box', BBBLC, BBTRC, 'InitialDirty', BLC, TRC)
C
         CALL MSGPUT ('Window contracted to mask','I')
         WRITE (MESSAGE, 1005) BLC(1), BLC(2), BLC(3)
 1005    FORMAT (' BLC = ',I4,',',I4,',',I4)
         CALL MSGPUT(MESSAGE,'I')
         WRITE (MESSAGE, 1006) TRC(1), TRC(2), TRC(3)
 1006    FORMAT (' TRC = ',I4,',',I4,',',I4)
         CALL MSGPUT(MESSAGE,'I')
      END IF
C
C Section up the images as needed
C
      IF(.NOT.IMGPLANE) THEN
         CALL DATCREAT ('Window')
         CALL CRDHALF ('InitialDirty', BLC, TRC)
         CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
         CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
         CALL IMGSUBSE ('InitialDirty', 'Dirty', 'Window')
         CALL DATDELET ('InitialDirty')
         IF (DOSOFTW) THEN
            CALL DATRENAM ('Box', 'InitialBox')
            CALL IMGSUBSE ('InitialBox', 'Box', 'Window')
            CALL DATDELET ('InitialBox')
         END IF
      ELSE
         CALL DATRENAM ('InitialDirty', 'Dirty')
      END IF
C
C Find image size
C
      CALL DATGETAR ('Dirty', NAX, NAXIS, ATYPE, AADD)
      IF (ERROR) GO TO 999
      IF (DOSOFTW) THEN
         CALL ARRSTAT ('Box', ' ')
         IMSZ = NINT(DATFGETR ('Box', 'ARRSUM'))
         WRITE (MESSAGE, 1010) IMSZ
 1010    FORMAT ('Soft Window has',I7,' pixels')
      ELSE
         IMSZ = 1
         DO 10 IAX = 1, NAX
            IMSZ = IMSZ * MAX(1, NAXIS(IAX))
 10      CONTINUE
         WRITE (MESSAGE, 1011) IMSZ
 1011    FORMAT (I8, ' pixels in image')         
      END IF
      CALL MSGPUT (MESSAGE, 'I')
C
C Fit a Gaussian Beam to PSF
C
      CALL FILBEMGE ('PSF', BEAM, FITALG, 'BEAM')
C
C Now make transfer functions, etc.
C
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL DATDELET ('PSF')
      CALL IMGCLONE ('XFR', 'VMVis')
      IF (FILEXIST (VMFILE)) THEN
         CALL FILIMGGE ('VM', VMFILE, ' ')
         IF (ENTTYPE(1:1).EQ.'H') THEN
            CALL ARRCLIP ('VM', MINFLUX, MAXFLUX, 'VM')
         END IF
         BITER = 0
         IF (DATEXIST('VM/NITER'))
     $      CALL DATGETI ('VM', 'NITER', BITER, 1, NDUMMY)
         WRITE (MESSAGE, 1200) BITER
 1200    FORMAT ('Restarting VM from iteration ',I4)
         CALL MSGPUT (MESSAGE, 'I')
         IF (DATEXIST('VM/Q')) CALL DATGETR ('VM', 'Q', Q, 1, NDUMMY)
      ELSE
         BITER = 0
         CALL IMGCLONE ('Dirty', 'VM')
         IF (ENTTYPE(1:1).EQ.'H') THEN
            CALL ARRCLIP ('VM', MINFLUX, MAXFLUX, 'VM')
            IF (TFLUX.NE.0.0) THEN
               CALL ARRSETCO ('VM', 0.0, ABS(TFLUX)/IMSZ)
            ELSE
               CALL ARRSETCO ('VM', 0.0, SIGMA/10.0)
            END IF
         ELSE
            CALL ARRSETCO ('VM', 0.0, 0.0)
         END IF
      END IF
C
C Clone some images
C
      CALL IMGCLONE ('Dirty', 'Step')
      CALL IMGCLONE ('Dirty', 'Residual')
C
C Now take care of default image
C
      IF (FILEXIST(DEFFILE)) THEN
         CALL FILIMGGE ('Default', DEFFILE, ' ')
         CALL ARRCLIP ('Default', MINFLUX, MAXFLUX, 'Default')
         IF(ENTTYPE(1:1).EQ.'H') THEN
            DEFLEV = 0.0
         ELSE
            DEFLEV = -MAXFLUX
         END IF
         CALL MSGPUT ('Using specified image as default', 'I')
         CALL ARRCOPY ('Default', 'VM')
      ELSE
         IF(ENTTYPE(1:1).EQ.'H') THEN
            IF (TFLUX.NE.0.0) THEN
               DEFLEV = ABS(TFLUX)/IMSZ
            ELSE
               DEFLEV = SIGMA / 10.0
            END IF
            CALL MSGPUT ('Using flat default', 'I')
         ELSE
            CALL MSGPUT ('Using blank default', 'I')
            DEFLEV = 0.0
         END IF
      END IF
C
C Do we want to use an ICF?
C
      DOICF = (ICF(1).NE.0.0).AND.(ICF(2).NE.0.0)
      IF(DOICF) THEN
         CALL MSGPUT ('Using ICF', 'I')
         CALL IMGCLONE('VM', 'ICFVM')
      END IF
C
C Create working images for soft window
C
      IF (DOSOFTW) THEN
         CALL DATRENAM ('VM', 'FullVM')
         CALL DATRENAM ('Step', 'FullStep')
         CALL DATRENAM ('Residual', 'FullResidual')
         CALL ARRSCOPY ('FullVM', 'Box', 'VM')
         CALL ARRSCOPY ('FullStep', 'Box', 'Step')
         CALL ARRSCOPY ('FullResidual', 'Box', 'Residual')
         CALL HEDCOPY ('FullVM', 'VM')
         CALL HEDCOPY ('FullStep', 'Step')
         CALL HEDCOPY ('FullResidual', 'Residual')
         IF (DATEXIST('Default')) THEN
            CALL DATRENAM ('Default', 'FullDefault')
            CALL ARRSCOPY ('FullDefault', 'Box', 'Default')
            CALL HEDCOPY ('FullDefault', 'Default')
            CALL DATDELET ('FullDefault')
         END IF
      END IF
C
C *************** Perform an iteration & write statistics ***************
C
      ITER = BITER 
      IF(ENTTYPE(1:1).EQ.'H') THEN
         CALL MSGPUT (
     1'Iteration          Entropy        Flux        Fit     Gradient',
     2      'I')
      ELSE
         CALL MSGPUT (
     1      'Iteration     Flux        Fit     Gradient',  'I')
      END IF
      FINISH = F
      CNVRGE = F
  200 CONTINUE
      CALL DATPUTR ('Residual', 'Q', Q, 1)
      IF (IMGPLANE) THEN
         TCHISQ = SIGMA **2 * IMSZ
      ELSE
         TCHISQ = SIGMA **2 * IMSZ / Q
      END IF
      IF(ENTTYPE(1:1).EQ.'H') THEN
         CALL MEMONEIT (ITER.EQ.BITER, 'VM', 'Default', DEFLEV, 'Step',
     $      'Residual', Q, TFLUX, TCHISQ, TOL, FLUX, CHISQ, ENTRPY, 
     $      NRMGRD, IMGCLRES)
         NOISE = SQRT(CHISQ/TCHISQ)
         ITER = ITER + 1
         WRITE (MESSAGE, 1100) ITER, ENTRPY, FLUX, NOISE, NRMGRD
 1100    FORMAT (1X,I5,5X,F16.3,2X,3(F10.4,1X))
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         NOISE = SQRT(CHISQ/TCHISQ)
         AFIT =  MAX(NOISE, 1.0) * SQRT(TCHISQ/IMSZ)
         CALL MUMONEIT (ITER.EQ.BITER, 'VM', 'Default', DEFLEV, 'Step',
     $      'Residual', Q, TFLUX, 0.0, TOL, FLUX, CHISQ, ENTRPY, 
     $      NRMGRD, AFIT, IMGCLRES)
         NOISE = SQRT(CHISQ/TCHISQ)
         ITER = ITER + 1
         WRITE (MESSAGE, 1101) ITER, FLUX, NOISE, NRMGRD
 1101    FORMAT (1X,I5,5X,3(F10.4,1X))
         CALL MSGPUT (MESSAGE, 'I')
      END IF
      CALL DATPUTR ('VM', 'Q', Q, 1)
      IF (ITER.GE.VMLIM) FINISH = T
      IF (ERROR) GO TO 999
C
C Write TV file if required
C
      IF((TVFILE.EQ.'VM').OR.(TVFILE.EQ.'Step').OR.
     $   (TVFILE.EQ.'Residual')) THEN
         IF (DOSOFTW) THEN
            TMPSTR = 'Full' // TVFILE
            CALL ARRSETCO (TMPSTR, 0.0, 0.0)
            CALL ARRUSCPY (TVFILE, 'Box', TMPSTR)
         ELSE
            TMPSTR = TVFILE
         END IF
         CALL IMGTVD (TMPSTR, 0.0, 0.0)
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
            VMLIM = 5
            DOSTOP = NITER.LT.0.0
            IF (NITER.NE.0) VMLIM = IABS(NITER)
            CALL USRGETR ('Sigma', NSIGMA, 1, NDUMMY)
            IF (NSIGMA.LE.0.0) THEN
               NSIGMA = 0.01
            END IF
            SIGMA = NSIGMA
            CALL DATPUTR ('Residual', 'SIGMA', SIGMA, 1)
            IF (DOSOFTW)
     $         CALL DATPUTR ('FullResidual', 'SIGMA', SIGMA, 1)
            CALL USRGETR ('Tflux', TFLUX, 1, NDUMMY)
            CALL USRGETC ('TV', TVFILE, 1, NDUMMY)
         END IF
      END IF
C                              Check if it's time to stop
      IF(ENTTYPE(1:1).EQ.'H') THEN
         CNVRGE = (NOISE.LT.(1.0+TOLER)) .AND. 
     1      (ITER.NE.(BITER+1)) .AND. (NRMGRD.LT.TOLER)
      ELSE
         CNVRGE = (NOISE.LT.TOLER) .AND. 
     1      (ITER.NE.(BITER+1)) .AND. (NRMGRD.LT.TOLER)
      END IF
      IF (TFLUX.GT.0.0) CNVRGE =  CNVRGE .AND. 
     1    (ABS(FLUX-TFLUX).LT.TOLER*TFLUX)
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
      IF (.NOT.FINISH) GO TO 200
C
C ************************ End of Iteration ************************
C
C Housekeeping
C
      IF (DATEXIST('Default'))     CALL DATDELET ('Default')
      IF (DATEXIST('FullDefault')) CALL DATDELET ('FullDefault')
      IF (DATEXIST('Step'))        CALL DATDELET ('Step')
      IF (DATEXIST('FullStep'))    CALL DATDELET ('FullStep')
C
      IF (DOSOFTW) THEN
         CALL ARRSETCO ('FullVM', 0.0, 0.0)
         CALL ARRUSCPY ('VM', 'Box', 'FullVM')
         CALL DATDELET ('VM')
         CALL DATDELET ('Residual')
         CALL DATRENAM ('FullVM', 'VM')
         CALL DATRENAM ('FullResidual', 'Residual')
      END IF
C
C Apply ICF
C
      IF (DOICF) THEN
         CALL IMGSMOOT ('VM', ICF, 'VM', 'VMVis')
      END IF
C
C Write answer to history
C
      CALL HISINPUT ('VM')
      CALL DATPUTI ('VM', 'NITER', ITER, 1)
      CALL HISOPEN ('VM')
      CALL HISPUT ('VM', 
     1   'Iteration      Entropy     Flux        Fit     Gradient')
      WRITE (MESSAGE, 1100) ITER, ENTRPY, FLUX, NOISE, NRMGRD
      CALL HISPUT ('VM', MESSAGE)
C
C Now write output image
C
      IF (VMFILE.NE.' ' ) THEN         
         CALL DATPUTC ('VM', 'BUNIT', 'JY/PIXEL', 1)
         CALL FILIMGPU ('VM', VMFILE, ' ')
      END IF
C
C If required, make smoothed version and add residuals
C
      IF (CVMFILE.NE.' ') THEN
         CALL IMGRESID ('VM', 'Dirty', 'XFR', 'Residual', 'VMVis')
         CALL IMGCLONE ('VM', 'CVM')
         CALL DATGETC ('Dirty', 'BUNIT', BUNIT, 1, NDUMMY)
         IF (ERROR) THEN
            CALL ERRCANCE
            BUNIT = 'JY/BEAM'
         END IF
         CALL IMGSMOOT ('VM', BEAM, 'CVM', 'VMVis')
         IF (BUNIT(1:7).NE.'JY/BEAM') THEN
            CALL MSGPUT ('Unit volume PSF assumed', 'I')
         ELSE
            CALL IMGP2PB ('CVM', BEAM, 'CVM')
         END IF
         CALL DATPUTC ('CVM', 'BUNIT', BUNIT, 1)
         CALL ARRLC ('CVM', 1.0, 'Residual', 1.0, 'CVM')
         IF (TVFILE.EQ.'CVM') THEN
            CALL IMGTVD (TVFILE, 0.0, 0.0)
         END IF
         CALL FILIMGPU ('CVM', CVMFILE, ' ')
      END IF
C
      IF (RESFILE.NE.' ') THEN
         CALL DATPUTC ('Residual', 'BUNIT', 'JY/BEAM', 1)
         CALL FILIMGPU ('Residual', RESFILE, ' ')
      END IF
C
 999  CONTINUE
      END
C
      SUBROUTINE IMGCLRES (MODEL, RES, CHISQ)
C
C Calculate Residual image (OBS - PRED) and CHISQ. This routine is 
C called by VM routines.
C
C
C	MODEL	CH*(*)	input	Name of model image
C	CHISQ	REAL	output	Chi-squared
C	RES	CH*(*)	input	Name of grad Chi-squared image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Soft windows added
C				D.S.Briggs	Mar 28 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, RES
      REAL		CHISQ
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCLRES')
C
      INTEGER		NDUMMY
      REAL		RMS, Q, ICF(4)
      LOGICAL		IMGPLANE, DOICF, DOSOFTW
      CHARACTER*(SYSMXNAM)	TRES, TMODEL
      COMMON		/VMCOM/	IMGPLANE, DOICF, ICF, DOSOFTW
C==================================================================
      IF (ERROR) GO TO 999
C
      IF (DOSOFTW) THEN
         TRES = 'FullResidual'
         TMODEL = 'FullVM'
         CALL ARRSETCO (TMODEL, 0.0, 0.0)
         CALL ARRUSCPY (MODEL, 'Box', TMODEL)
      ELSE
         TRES = RES
         TMODEL = MODEL
      END IF
C
      CALL DATGETR (RES, 'Q', Q, 1, NDUMMY)
C
C Find residuals
C
      IF (DOICF) THEN
         CALL IMGSMOOT (TMODEL, ICF, 'ICFVM', 'VMVis')
         CALL IMGRESID ('ICFVM', 'Dirty', 'XFR', TRES, 'VMVis')
      ELSE
         CALL IMGRESID (TMODEL, 'Dirty', 'XFR', TRES, 'VMVis')
      END IF
      IF (DOSOFTW) THEN
         CALL ARRSETCO (RES, 0.0, 0.0)
         CALL ARRSCOPY (TRES, 'Box', RES)
      END IF
      CALL ARRRMS (RES, RMS, CHISQ)
      IF (IMGPLANE) THEN
         CALL IMGFFT (RES, 'VMVis')
         CALL ARRMULT ('XFR', 'VMVis', 'VMVis')
         CALL IMGFFT ('VMVis', RES)
      ELSE
         CHISQ = CHISQ / Q
      END IF
      IF(DOICF) THEN
         IF (DOSOFTW) THEN
            CALL IMGSMOOT (TRES, ICF, TRES, 'VMVis')
            CALL ARRSETCO (RES, 0.0, 0.0)
            CALL ARRSCOPY (TRES, 'Box', RES)
         ELSE
            CALL IMGSMOOT (RES, ICF, RES, 'VMVis')
         END IF
      END IF
      IF (ERROR) GO TO 990
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
