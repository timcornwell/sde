C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C
C Various defines
C
#define ntel 20
C
      SUBROUTINE SDEMAIN
C
C Program to perform maximum entropy deconvolution. This version
C calculates residuals in the true visibility-plane or in the
C image plane, as requested. This version is horribly inefficient
C if run with a large number of pointing in ImagePlane mode since
C transforms of the full field are done even though most of the field
C is empty. It is better then to use DFT mode.
C One day we should re-write this the in style of fly so that image
C links are used to clean up the organization.
C
C Audit trail:
C	In the MOSCAL?? routines, the pointing center of 'Denom' was
C	not being set correctly. This would not have affected most cases.
C				T.J.Cornwell	Jan 25 1989
C	Added SDETYPE
C				T.J. Cornwell	Feb 3 1989
C       Added different SIGMA for each pointing
C                              T.J. Cornwell   May 24 1989
C       Now can read both MOS and VIS files
C                              T.J. Cornwell   May 30 1989
C	Added beam-fitting
C				T.J. Cornwell	Oct 5 1989
C	The program was asking for too many SIGMA1's
C				T.J. Cornwell	Oct 17 1989
C	Now writes out VM, CVM images every WITER iterations
C				M.A.Holdaway	Jan 25 1990
C	Added emptiness
C				T.J. Cornwell	Feb 1, 1990
C	Added Q,U,V processing
C				T.J. Cornwell	March 30, 1990
C	DFT is now a string of telescopes which need DFT handling
C				T.J. Cornwell	April 17 1990
C	Changed to a single SIGMA
C				T.J. Cornwell	April 18 1990
C	Fixed SIGMA error
C				T.J. Cornwell	May 11 1990
C	Allowed sigma per telescope
C				T.J. Cornwell	May 27 1990
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSAIC')
C
      INTEGER		NITER, BITER, ITER, NDUMMY, IMSZ, IAX,
     1			VMLIM, WITER
      REAL		CELLSIZE(3), TAPER(3),
     1			TIME(2), UVLIMITS(2), SHIFT(3)
      INTEGER 		DIR, NSEL, TIMR(8), IMSIZE(3), NPC, IPC,
     1			FFTPWR2, PIMSIZE(3), 
     2			BLC (SYSMXDIM), TRC (SYSMXDIM)
      CHARACTER*(SYSMXNAM)	CTIME, SUBCLASS
      REAL		DEFLEV, TFLUX, TCHISQ, FLUX, CHISQ, ENTRPY, TOL, 
     1			NRMGRD, Q, NOISE, TOLER, BEAM(4), FOV, SERR
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      CHARACTER*6	STRINT
      REAL		SIGMA, DATFGETR, AFIT, SIGTEL(ntel)
      CHARACTER*(SYSMXNAM)	VMFILE, DEFFILE, 
     1				IVIS, VIS, MOSFILE, CVMFILE, ENTTYPE
      LOGICAL		FILEXIST, DOTV, NICEPSF
      LOGICAL		DOSTOP, FINISH, CNVRGE, T, F, IMGPLANE
      CHARACTER*(SYSMXNAM)	STOKES, DFT(ntel), TELNAMES(ntel)
      INTEGER		NDFT, ITEL, STRSEARC, STRLEN
      COMMON		/MOSCOM/	IMGPLANE, NDFT
      COMMON		/MOSCOC/	STOKES, DFT
      EXTERNAL		MOSCLRES
      CHARACTER*(SYSMXNAM)	STRM2, TELESCOP
      DATA		T	/.TRUE./
      DATA		F	/.FALSE./
      DATA		BLC	/SYSMXDIM*1/
      DATA		TRC	/SYSMXDIM*1/
C==================================================================
      CALL MSGWELCO ('I perform maximum entropy mosaicing')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 3, NDUMMY)
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('Filter', TAPER, 3, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETL ('ImagePlane', IMGPLANE, 1, NDUMMY)
      CALL USRGETC ('Mosaic', MOSFILE, 1, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETC ('Default', DEFFILE, 1, NDUMMY)
      CALL USRGETC ('VM', VMFILE, 1, NDUMMY)
      CALL USRGETC ('CVM', CVMFILE, 1, NDUMMY)
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETI ('Write', WITER, 1, NDUMMY)
      CALL USRGETR ('Tflux', TFLUX, 1, NDUMMY)
      CALL USRGETR ('Sigma', SIGMA, 1, NDUMMY)
      CALL USRGETC ('Telnames', TELNAMES, ntel, NDUMMY)
      CALL USRGETR ('Sigtel', SIGTEL, ntel, NDUMMY)
      CALL USRGETC ('Entropy', ENTTYPE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETR ('Tolerance', TOLER, 1, NDUMMY)
      CALL USRGETR ('Npoints', Q, 1, NDUMMY)
      CALL USRGETR ('Gain', TOL, 1, NDUMMY)
      CALL USRGETL ('NicePSF', NICEPSF, 1, NDUMMY)
      CALL USRGETC ('DFT', DFT, ntel, NDFT)
      CALL USRGETL ('Dotv', DOTV, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
      IF (ENTTYPE(1:1).EQ.'H') THEN
         CALL MSGPUT ('Maximising entropy -I*logI', 'I')
      ELSE
         CALL MSGPUT ('Maximising emptiness', 'I')
      END IF
C
C Process input parameters
C
      VMLIM = 5
      DOSTOP = NITER.LT.0.0
      IF (NITER.NE.0) VMLIM = IABS(NITER)
C
C Subclass to be gridded
C
      CALL MSGPUT ('Working on Stokes type '//STOKES(1:1), 'I')
      SUBCLASS = 'OBS/'//STOKES(1:1)
C
C Find image size
C
      IF (ERROR) GO TO 999
      IMSZ = 1
      DO 10 IAX = 1, 3
         IMSZ = IMSZ * MAX(1, IMSIZE(IAX))
  10  CONTINUE
C
C Now get relevant files
C
      NPC = 0
      CALL DATCREAT('IM')
      IF (MOSFILE.NE.' ') THEN
         CALL MSGPUT ('Reading Mosaic visibility file', 'I')
         CALL VISMOSGE ('IM', MOSFILE)
C
         CALL DATGETI ('IM', 'NPC', NPC, 1, NDUMMY)
         IF (NPC.EQ.0) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'No pointings')
            GO TO 999
         END IF
      END IF
      CALL DATPUTI ('IM', 'NPC', NPC, 1)
      IF(NPC.EQ.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'You must specify an input file via Mosaic')
         GO TO 999
      ELSE
         WRITE (MESSAGE, 1050) NPC
 1050    FORMAT ('There are ',I4,' pointings in all')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
      IF (SIGMA.LE.0.0) SIGMA = .001
C
C Now select data
C
      CALL UTLTTOD (TIMR(1), TIME(1))
      CALL UTLTTOD (TIMR(5), TIME(2))
      CALL DATCREAT('M')
      CALL DATPUTI ('M', 'NPC', NPC, 1)
      SERR = 0.0
      DO 1 IPC = 1, NPC
         IVIS = 'IM/PC'//STRINT(IPC)
         VIS = 'M/PC'//STRINT(IPC)
         CALL VISSEL (IVIS, SUBCLASS, TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) IPC, NSEL
 1000    FORMAT ('Pointing ',I4,' : selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
         CALL VISSUB (IVIS, 'OBS', STOKES, VIS)
         CALL DATDELET (IVIS)
         CALL DATGETC (VIS, 'TELESCOP', TELESCOP, 1, NDUMMY)
         ITEL = STRSEARC(TELESCOP,TELNAMES,ntel)
         IF (ITEL.NE.0) THEN
            IF(SIGTEL(ITEL).LE.0.0) SIGTEL(ITEL) = SIGMA
            WRITE (MESSAGE, 2000) IPC,
     $         TELNAMES(ITEL)(1:STRLEN(TELNAMES(ITEL))), SIGTEL(ITEL)
 2000       FORMAT ('Pointing ',I4,' is telescope ',A,' and has ',
     $         'sigma = ',F12.4,' Jy/beam')
            CALL MSGPUT (MESSAGE, 'I')
            SERR = SERR + IMSZ * SIGTEL(ITEL)**2
            CALL DATPUTR (VIS, 'SIGMA', SIGTEL(ITEL), 1)
         ELSE
            SERR = SERR + IMSZ * SIGMA**2
            CALL DATPUTR (VIS, 'SIGMA', SIGMA, 1)
         END IF
   1  CONTINUE
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got data: '//CTIME, 'I')
C
C Get images. First try to get existing VM image. If it does not
C exist then create it.
C
      IF (FILEXIST(VMFILE)) THEN
         CALL FILIMGGE ('VM', VMFILE, ' ')
         IF (ENTTYPE(1:1).EQ.'H') THEN
            CALL ARRCLIP ('VM', 1E-8, 1E20, 'VM')
         END IF
         CALL DATGETI ('VM', 'NITER', BITER, 1, NDUMMY)
         IF (ERROR) THEN
            CALL ERRCANCE
            BITER = 0
         END IF
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1200) BITER
 1200    FORMAT ('Restarting VM from iteration ',I4)
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         BITER = 0
         IF (ERROR) GO TO 999
         CALL MSGPUT ('Creating new VM image', 'I')
         CALL IMGMAKE ('M/PC1/'//SUBCLASS, CELLSIZE, IMSIZE, SHIFT,
     1      'R', 'VM')
         IF (ENTTYPE(1:1).EQ.'H') THEN
            IF (TFLUX.NE.0.0) THEN
               CALL ARRSETCO ('VM', ABS(TFLUX), 0.0)
            ELSE
               CALL ARRSETCO ('VM', 0.0, (SIGMA/10.0))
            END IF
         ELSE
            CALL ARRSETCO ('VM', 0.0, 0.0)
         END IF
         IF (ERROR) GO TO 999
      END IF
      CALL DATGETD ('VM', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
      CALL DATPUTD ('VM', 'OBSRA', RVAL(1), 1)
      CALL DATPUTD ('VM', 'OBSDEC', RVAL(2), 1)
      CALL DATPUTC ('VM', 'BUNIT', 'JY/PIXEL', 1)
C
C Find dual image: Pad by factor of two e.g. 60 -> 128
C
      IF (IMGPLANE) THEN
         PIMSIZE(1) = 2*FFTPWR2(IMSIZE(1))
         PIMSIZE(2) = 2*FFTPWR2(IMSIZE(2))
      ELSE
         PIMSIZE(1) = FFTPWR2(IMSIZE(1))
         PIMSIZE(2) = FFTPWR2(IMSIZE(2))
      END IF
      PIMSIZE(3) = 1
      CALL IMGMAKE (STRM2('M/PC1', SUBCLASS), CELLSIZE, PIMSIZE, 
     1   SHIFT, 'R', 'PSF')
      CALL FFTCONJA ('PSF', 'VMVis', DIR, 0)
C
C Make some visibility files
C
      IF (.NOT.IMGPLANE) THEN
         DO 4 IPC = 1, NPC
            VIS = 'M/PC'//STRINT(IPC)
            CALL VISCLONE (VIS, 'OBS', STOKES(1:1), 'MOD')
            IF (ERROR) GO TO 999
  4      CONTINUE
      END IF
      IF (ERROR) GO TO 999
      CALL MSGPUT ('Coordinates for VM:', 'I')
      CALL CRDLIST ('VM')
         IF (ERROR) GO TO 999
      CALL MSGPUT ('Coordinates for transform of VM:', 'I')
      CALL CRDLIST ('VMVis')
      IF (ERROR) GO TO 999
C
C Now do uniform weighting if required
C
      IF (NICEPSF.OR.(FOV.NE.0.0)) THEN
         CALL MSGPUT ('Reweighting data', 'I')
         DO 2 IPC = 1, NPC
            VIS = 'M/PC'//STRINT(IPC)
            IF(NICEPSF) THEN
               CALL IMGMSPSF (VIS, SUBCLASS, 'VMVis', 'PSF')
            ELSE
               CALL DATPUTR ('VMVis', 'WTFOV', FOV, 1)
               CALL GRDUWT (VIS, SUBCLASS, 'VMVis')
            ENDIF
            CALL ARRCOPY (STRM2(VIS, 'OBS/'//STOKES(1:1)//'/WT'), 
     1         STRM2(VIS, 'MOD/'//STOKES(1:1)//'/WT'))
            IF (ERROR) GO TO 999
  2      CONTINUE
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Finished reweighting: '//CTIME, 'I')
      END IF
C
C Clone some convenient stuff
C
C
C Apply taper
C
      IF ((TAPER(1).NE.0.0).OR.(TAPER(2).NE.0.0)) THEN
         DO 6 IPC = 1, NPC
            VIS = 'M/PC'//STRINT(IPC)
            CALL VISTAPER (VIS, SUBCLASS, TAPER, SUBCLASS)
            CALL ARRCOPY (STRM2(VIS, 'OBS/'//STOKES(1:1)//'/WT'), 
     1         STRM2(VIS, 'MOD/'//STOKES(1:1)//'/WT'))
            IF (ERROR) GO TO 999
 6       CONTINUE
         IF (ERROR) GO TO 999
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Filtering: '//CTIME, 'I')
      END IF
C
C Clone some stuff
C
      CALL IMGCLONE ('VM', 'Step')
      CALL IMGCLONE ('VM', 'Residual')
      CALL IMGCLONE ('VM', 'TmpVM')
C
C Now take care of default image
C
      IF (FILEXIST(DEFFILE)) THEN
         CALL FILIMGGE ('Default', DEFFILE, ' ')
         CALL ARRCLIP ('Default', 1E-8, 1E20, 'Default')
         CALL ARRCOPY ('Default', 'VM')
         IF(ENTTYPE(1:1).EQ.'H') THEN
            DEFLEV = 0.0
         ELSE
            DEFLEV = -1E20
         END IF
         CALL MSGPUT ('Using specified image as default', 'I')
         CALL MSGPUT ('Initial guess is the default', 'I')
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
         CALL MSGPUT ('Initial guess is a flat image', 'I')
      END IF
      IF (ERROR) GO TO 999
C
C If we want to work in the image plane then make the dirty maps
C and XFRs
C
      IF (IMGPLANE) THEN
         CALL MSGPUT ('Making dirty images and transfer functions',
     1      'I')
         CALL DATCREAT ('Window')
         BLC(1) = PIMSIZE(1)/2 - IMSIZE(1) / 2 + 1
         BLC(2) = PIMSIZE(2)/2 - IMSIZE(2) / 2 + 1
         TRC(1) = PIMSIZE(1)/2 + IMSIZE(1) / 2 
         TRC(2) = PIMSIZE(2)/2 + IMSIZE(2) / 2 
         CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
         CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
         DO 7 IPC = 1, NPC
            VIS = 'M/PC'//STRINT(IPC)
            CALL IMGCLONE ('VMVis', STRM2(VIS, 'XFR'))
            CALL VISGRID (VIS, SUBCLASS, STRM2(VIS, 'XFR'), .TRUE.)
            CALL IMGFFT (STRM2(VIS, 'XFR'), 'PSF')
            CALL IMGGRIDC ('PSF', 'PSF', 'CORRECT')
            CALL IMGFFT ('PSF', STRM2(VIS, 'XFR'))
C
C Fit a Gaussian Beam to PSF
C
            IF ((IPC.EQ.1).AND.(BEAM(1) .EQ. 0.)) THEN
               CALL IMGBMSHP( 'PSF' )
               IF (ERROR) GO TO 999
               BEAM(1) =  DATFGETR( 'PSF', 'BMAJ')*3600.0
               BEAM(2) =  DATFGETR( 'PSF', 'BMIN')*3600.0
               BEAM(3) =  DATFGETR( 'PSF', 'BPA')
               BEAM(4) =  DATFGETR( 'PSF', 'BZ')*3600.0
            ENDIF
            CALL IMGMAKE (STRM2(VIS, SUBCLASS), CELLSIZE, IMSIZE, SHIFT,
     1         'R', STRM2(VIS, 'Dirty'))
            CALL VISGRID (VIS, SUBCLASS, 'VMVis', .FALSE.)
            CALL IMGFFT ('VMVis', 'PSF')
            CALL IMGGRIDC ('PSF', 'PSF', 'CORRECT')
            CALL IMGSUBSE ('PSF', STRM2(VIS, 'Dirty'), 'Window')
  7      CONTINUE
         CALL DATDELET ('PSF')
      ELSEIF (BEAM(1).EQ.0.0) THEN
         CALL MSGPUT ('Making first dirty beam only', 'I')
         VIS = 'M/PC1'
         CALL VISGRID (VIS, SUBCLASS, 'VMVis', .TRUE.)
         CALL IMGFFT ('VMVis', 'PSF')
         CALL IMGGRIDC ('PSF', 'PSF', 'CORRECT')
         CALL IMGBMSHP( 'PSF' )
         IF (ERROR) GO TO 999
         BEAM(1) =  DATFGETR( 'PSF', 'BMAJ')*3600.0
         BEAM(2) =  DATFGETR( 'PSF', 'BMIN')*3600.0
         BEAM(3) =  DATFGETR( 'PSF', 'BPA')
         BEAM(4) =  DATFGETR( 'PSF', 'BZ')*3600.0
         CALL DATDELET ('PSF')
      END IF
C
C Initial CHISQ
C
      CALL DATPUTR ('Residual', 'Q', Q, 1)
      CALL MOSCLRES ('VM', 'Residual', CHISQ)
      IF(ERROR) GO TO 999
      TCHISQ = SERR / Q
      NOISE = SQRT(CHISQ/TCHISQ)
      WRITE (MESSAGE, 1300) NOISE
 1300 FORMAT ('Initial fit = ',F10.3)
      CALL MSGPUT (MESSAGE, 'I')
C
C ************************ Perform iterations ***********************
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
      ITER = ITER + 1
      IF (ITER.GE.VMLIM) FINISH = T
      CALL DATPUTR ('Residual', 'Q', Q, 1)
      TCHISQ = SERR / Q
      IF(ENTTYPE(1:1).EQ.'H') THEN
         CALL MEMONEIT (ITER.EQ.BITER+1, 'VM', 'Default', DEFLEV,
     $      'Step', 'Residual', Q, TFLUX, TCHISQ, TOL, FLUX, CHISQ, 
     $      ENTRPY, NRMGRD, MOSCLRES)
         NOISE = SQRT(CHISQ/TCHISQ)
         WRITE (MESSAGE, 1100) ITER, ENTRPY, FLUX, NOISE, NRMGRD
 1100    FORMAT (1X,I5,5X,F16.3,1X,3(F10.3,1X))
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         AFIT =  MAX(NOISE, 1.0) * SQRT(SERR/ (NPC * IMSZ))
         CALL MUMONEIT (ITER.EQ.BITER+1, 'VM', 'Default', DEFLEV, 
     $      'Step', 'Residual', Q, TFLUX, 0.0, TOL, FLUX, CHISQ, 
     $      ENTRPY, NRMGRD, AFIT, MOSCLRES)
         NOISE = SQRT(CHISQ/TCHISQ)
         WRITE (MESSAGE, 1101) ITER, FLUX, NOISE, NRMGRD
 1101    FORMAT (1X,I5,5X,3(F10.3,1X))
         CALL MSGPUT (MESSAGE, 'I')
      END IF
      IF (ERROR) GO TO 999
C
      IF (DOTV) CALL IMGTVD ('VM', 0.0, 0.0)
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
            CALL USRGETR ('Tflux', TFLUX, 1, NDUMMY)
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
      IF (CNVRGE) THEN
         IF (DOSTOP) THEN
            FINISH = T
         ELSE 
            TOL = NRMGRD
         END IF
      END IF
C
C Write answer if finished or if ITER = N * WITER
C
      IF (MOD(ITER, WITER) .EQ. 0) THEN
         CALL DATPUTI ('VM', 'NITER', ITER, 1)
         IF(VMFILE.NE.' ') THEN
            CALL FILIMGPU ('VM', VMFILE, ' ')
         END IF
      ENDIF
      IF (.NOT.FINISH) GO TO 200
C
C ************************ End of Iteration ************************
C
C
      CALL DATPUTI ('VM', 'NITER', ITER, 1)
      CALL HISOPEN ('VM')
      CALL HISINPUT ('VM')
      IF(ENTTYPE(1:1).EQ.'H') THEN
         CALL HISPUT ('VM',
     1'Iteration          Entropy        Flux        Fit     Gradient')
         WRITE (MESSAGE, 1100) ITER, ENTRPY, FLUX, NOISE, NRMGRD
         CALL HISPUT ('VM', MESSAGE)
      ELSE
         CALL HISPUT ('VM',
     1      'Iteration     Flux        Fit     Gradient')
         WRITE (MESSAGE, 1101) ITER, FLUX, NOISE, NRMGRD
         CALL HISPUT ('VM', MESSAGE)
      END IF
C
C Now write output image
C
      IF (VMFILE.NE.' ' ) THEN         
         CALL FILIMGPU ('VM', VMFILE, ' ')
      END IF
C
C Restore image if required
C
      IF (CVMFILE.NE.' ') THEN
         CALL MOSCALRR ('VM', 'Residual')
         CALL IMGCLONE ('VM', 'CVM')
         CALL IMGSMOOT ('VM', BEAM, 'CVM', 'VMVis')
         CALL IMGP2PB ('CVM', BEAM, 'CVM')
         CALL ARRADD ('CVM', 'Residual', 'CVM')
         CALL FILIMGPU ('CVM', CVMFILE, ' ')
      END IF
C

 999  CONTINUE
      END
C++
C
      SUBROUTINE MOSCLRES (MODEL, RES, CHISQ)
C
C Calculate Chi-squared and grad Chi-squared for visibility
C data. This routine is called by VM routines. WORKS in
C uv plane or image plane as directed
C
C
C	MODEL	CH*(*)	input	Name of model image
C	CHISQ	REAL	output	Chi-squared
C	RES	CH*(*)	input	Name of grad Chi-squared image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, RES
      REAL		CHISQ
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSCLRES')
C
      INTEGER		NDUMMY, IPC, NPC, STRSEARC
      REAL		DBCHISQ, Q, RMS, WT, SUMWT, SIGMA
      DOUBLE PRECISION	OBSRA, OBSDEC
      CHARACTER*(SYSMXNAM) 	STRM2, TELESCOP, VIS
      CHARACTER*6	STRINT
      LOGICAL		IMGPLANE
      INTEGER		NDFT
      CHARACTER*(SYSMXNAM)	STOKES, DFT(ntel)
      COMMON		/MOSCOM/	IMGPLANE, NDFT
      COMMON		/MOSCOC/	STOKES, DFT
C==================================================================
      CHISQ = 0.0
      CALL DATGETR (RES, 'Q', Q, 1, NDUMMY)
C
      IF (ERROR) GO TO 999
C
C Find residuals
C
      CALL DATGETI ('M', 'NPC', NPC, 1, NDUMMY)
      SUMWT = 0.0
      DO 5 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL DATGETR (VIS, 'SIGMA', SIGMA, 1, NDUMMY)
         IF(SIGMA.LE.0.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Sigma zero')
            GO TO 990
         END IF
         IF(ERROR) GO TO 990
         SUMWT = SUMWT + 1.0 / SIGMA**2
 5    CONTINUE
      IF(SUMWT.LE.0.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Summed Weight zero')
         GO TO 990
      END IF
C
      DO 10 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL DATGETR (VIS, 'SIGMA', SIGMA, 1, NDUMMY)
         IF (ERROR) GO TO 990
         WT = 1.0 / (SUMWT*SIGMA**2)
         WT = 1.0
         IF(WT.LE.0.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Weight zero')
            GO TO 990
         END IF
         CALL DATGETD (VIS, 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATGETD (VIS, 'OBSDEC', OBSDEC, 1, NDUMMY)
         CALL DATGETC (VIS, 'TELESCOP', TELESCOP, 1, NDUMMY)
         CALL DATPUTD (MODEL, 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('TmpVM', 'OBSRA', OBSRA, 1)
         CALL DATPUTD (MODEL, 'OBSDEC', OBSDEC, 1)
         CALL DATPUTD ('TmpVM', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTC (MODEL, 'TELESCOP', TELESCOP, 1)
         CALL DATPUTC ('TmpVM', 'TELESCOP', TELESCOP, 1)
         IF (STRSEARC(TELESCOP,DFT,NDFT).GT.0) THEN
            CALL IMGDFTPB(VIS, 'MOD/'//STOKES(1:1), MODEL, 'TmpVM')
            CALL ARRSUBTR (STRM2(VIS, 'OBS/'//STOKES(1:1)//'/VIS'), 
     1         STRM2(VIS, 'MOD/'//STOKES(1:1)//'/VIS'),
     $         STRM2(VIS, 'MOD/'//STOKES(1:1)//'/VIS'))
            CALL VISDFTPB (VIS, 'MOD/'//STOKES(1:1), 'TmpVM', DBCHISQ)
            CALL ARRSCALE ('TmpVM', WT, 0.0, 'TmpVM')
            IF (IPC.EQ.1) THEN
               CALL ARRCOPY ('TmpVM', RES)
               CHISQ = WT * DBCHISQ / Q
            ELSE
               CALL ARRADD (RES, 'TmpVM', RES)
               CHISQ = CHISQ + WT * DBCHISQ / Q
            END IF
         ELSE
            CALL IMGPB (MODEL, 'TmpVM', 'APPLY')
            IF (IMGPLANE) THEN
               CALL IMGRESID ('TmpVM', STRM2(VIS, 'Dirty'), 
     1            STRM2(VIS, 'XFR'), 'TmpVM', 'VMVis')
            ELSE
               CALL IMGGRIDC ('TmpVM', 'TmpVM', 'CORRECT')
               CALL IMGFFT ('TmpVM', 'VMVis')
               CALL VISDEGRI (VIS, 'MOD/'//STOKES(1:1), 'VMVis')
               CALL ARRSUBTR (STRM2(VIS, 'OBS/'//STOKES(1:1)//'/VIS'), 
     1            STRM2(VIS, 'MOD/'//STOKES(1:1)//'/VIS'),
     $            STRM2(VIS, 'MOD/'//STOKES(1:1)//'/VIS'))
               CALL VISGRID (VIS, 'MOD/'//STOKES(1:1), 'VMVis', .FALSE.)
               CALL IMGFFT ('VMVis', 'TmpVM')
               CALL IMGGRIDC ('TmpVM', 'TmpVM', 'CORRECT')
            END IF
            CALL ARRRMS ('TmpVM', RMS, DBCHISQ)
            CALL ARRSCALE ('TmpVM', WT, 0.0, 'TmpVM')
            IF (IPC.EQ.1) THEN
               CALL IMGPB ('TmpVM', RES, 'APPLY')
               CHISQ = WT * DBCHISQ / Q
            ELSE
               CALL IMGPB ('TmpVM', 'TmpVM', 'APPLY')
               CALL ARRADD (RES, 'TmpVM', RES)
               CHISQ = CHISQ + WT * DBCHISQ / Q
            END IF
         END IF
  10  CONTINUE
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE MOSCALRR (MODEL, RES)
C
C Calculate Residual corrected for primary beam
C
C
C	MODEL	CH*(*)	input	Name of model image
C	RES	CH*(*)	input	Name of grad Chi-squared image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Fixed accumulation of squared primary beams. Previously
C	was not setting pointing center of PBFlat correctly.
C				T.J.Cornwell	March 10 1990
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, RES
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSCALRR')
C
      INTEGER		NDUMMY, IPC, NPC, STRSEARC
      REAL		DBCHISQ, MAXSEN, WT, SUMWT, SIGMA
      DOUBLE PRECISION	OBSRA, OBSDEC
C
      CHARACTER*(SYSMXNAM) 	STRM2, TELESCOP, VIS
      CHARACTER*6	STRINT
      LOGICAL		IMGPLANE
      INTEGER		NDFT
      CHARACTER*(SYSMXNAM)	STOKES, DFT(ntel)
      COMMON		/MOSCOM/	IMGPLANE, NDFT
      COMMON		/MOSCOC/	STOKES, DFT
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL IMGCLONE (MODEL, 'Flat')
      CALL ARRSETCO ('Flat', 0.0, 1.0)
      CALL IMGCLONE ('Flat', 'Denom')
      CALL ARRSETCO ('Denom', 0.0, 0.0)
      CALL IMGCLONE ('Denom', 'PBFlat')
      CALL ARRSETCO (RES, 0.0, 0.0)
C
C Find residuals
C
      CALL DATGETI ('M', 'NPC', NPC, 1, NDUMMY)
C
      SUMWT = 0.0
      DO 5 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL DATGETR(VIS, 'SIGMA', SIGMA, 1, NDUMMY)
         IF(SIGMA.LE.0.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Sigma zero')
            GO TO 990
         END IF
         IF(ERROR) GO TO 990
         SUMWT = SUMWT + 1.0 / SIGMA**2
 5    CONTINUE
      IF(SUMWT.LE.0.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Summed Weight zero')
         GO TO 990
      END IF
C
      DO 10 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL DATGETR (VIS, 'SIGMA', SIGMA, 1, NDUMMY)
         IF(ERROR) GO TO 990
         WT = 1.0 / (SUMWT*SIGMA**2)
         IF(WT.LE.0.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Weight zero')
            GO TO 990
         END IF
         CALL DATGETD (VIS, 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATGETD (VIS, 'OBSDEC', OBSDEC, 1, NDUMMY)
         CALL DATGETC (VIS, 'TELESCOP', TELESCOP, 1, NDUMMY)
         CALL DATPUTD (MODEL, 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('TmpVM', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Flat', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('PBFlat', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Denom', 'OBSRA', OBSRA, 1)
         CALL DATPUTD (MODEL, 'OBSDEC', OBSDEC, 1)
         CALL DATPUTD ('TmpVM', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTD ('Flat', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTD ('PBFlat', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTD ('Denom', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTC (MODEL, 'TELESCOP', TELESCOP, 1)
         CALL DATPUTC ('TmpVM', 'TELESCOP', TELESCOP, 1)
         CALL DATPUTC ('Flat', 'TELESCOP', TELESCOP, 1)
         CALL DATPUTC ('PBFlat', 'TELESCOP', TELESCOP, 1)
         CALL DATPUTC ('Denom', 'TELESCOP', TELESCOP, 1)
         IF (STRSEARC(TELESCOP,DFT,NDFT).GT.0) THEN
            CALL IMGDFTPB(VIS, 'MOD/'//STOKES(1:1), MODEL, 'TmpVM')
            CALL ARRSUBTR (STRM2(VIS, 'OBS/'//STOKES(1:1)//'/VIS'), 
     1         STRM2(VIS, 'MOD/'//STOKES(1:1)//'/VIS'),
     $         STRM2(VIS, 'MOD/'//STOKES(1:1)//'/VIS'))
            CALL VISDFTPB (VIS, 'MOD/'//STOKES(1:1), 'TmpVM', DBCHISQ)
         ELSE
            CALL IMGPB (MODEL, 'TmpVM', 'APPLY')
            IF (IMGPLANE) THEN
               CALL IMGRESID ('TmpVM', STRM2(VIS, 'Dirty'), 
     1            STRM2(VIS, 'XFR'), 'TmpVM', 'VMVis')
            ELSE
               CALL IMGGRIDC ('TmpVM', 'TmpVM', 'CORRECT')
               CALL IMGFFT ('TmpVM', 'VMVis')
               CALL VISDEGRI (VIS, 'MOD/'//STOKES(1:1), 'VMVis')
               CALL ARRSUBTR (STRM2(VIS, 'OBS/'//STOKES(1:1)//'/VIS'), 
     1            STRM2(VIS, 'MOD/'//STOKES(1:1)//'/VIS'),
     $            STRM2(VIS, 'MOD/'//STOKES(1:1)//'/VIS'))
               CALL VISGRID (VIS, 'MOD/'//STOKES(1:1), 'VMVis', .FALSE.)
               CALL IMGFFT ('VMVis', 'TmpVM')
               CALL IMGGRIDC ('TmpVM', 'TmpVM', 'CORRECT')
            END IF
            CALL IMGPB ('TmpVM', 'TmpVM', 'APPLY')
         END IF
C
C Now find sum of residuals and sum of primary beam squared
C
         CALL ARRLC (RES, 1.0, 'TmpVM', WT, RES)
         CALL IMGPB ('Flat', 'PBFlat', 'APPLY')
         CALL IMGPB ('PBFlat', 'PBFlat', 'APPLY')
         CALL ARRLC ('Denom', 1.0, 'PBFlat', WT, 'Denom')
C
  10  CONTINUE
C
C Now take ratio
C
      CALL ARRSTAT ('Denom', ' ')
      CALL DATGETR ('Denom', 'ARRMAX', MAXSEN, 1, NDUMMY)
      CALL ARRCDIV (RES, 'Denom', 1E-2*MAXSEN, RES)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
