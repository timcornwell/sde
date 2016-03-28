C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
C Various defines
C
C Maximum number of pointings to process: must agree with .inf file
C
#define MAXNPC 10
C
      SUBROUTINE SDEMAIN
C
CD Program to perform maximum entropy deconvolution. This version
CD calculates residuals in the true visibility-plane or in the
CD image plane, as requested. This version corrects for the rotation
CD of the Sun during the day.
C
CS Arguments: CALL SDEMAIN
CA Audit trail:
CA      Now can read both MOS and VIS files
CA                              T.J. Cornwell   May 30 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SUNMOSAIC')
C
      INTEGER		NITER, BITER, ITER, NDUMMY, IMSZ, IAX, NAX,
     1			NAXIS(SYSMXDIM), AADD, REALNAX, VMLIM
      REAL		CELLSIZE(3), TAPER(3),
     1			TIME(2), UVLIMITS(2), SHIFT(3)
      INTEGER 		DIR, NSEL, TIMR(8), IMSIZE(3), NPC, IPC,
     1			FFTPWR2, PIMSIZE(3), 
     2			BLC (SYSMXDIM), TRC (SYSMXDIM)
      CHARACTER*(SYSMXNAM)	CTIME, SUBCLASS
      REAL		DEFLEV, TFLUX, TCHISQ, FLUX, CHISQ, ENTRPY, TOL, 
     1			NRMGRD, Q, NOISE, TOLER,
     2			BEAM(4), SBEAM(4), BFACT, FACT, FOV
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), 
     1			ROTA(SYSMXDIM)
      CHARACTER*8       CTYPE(SYSMXDIM)
      CHARACTER*6	STRINT
      CHARACTER*1	ATYPE, TYPE
      REAL		SIGMA(MAXNPC), SRVAR, SOLRAD, PSUN, TREF, 
     $                  TRANGE(2), TSTEP, REFDATE
      CHARACTER*(SYSMXNAM)	VMFILE, DEFFILE, VISFILE(MAXNPC), 
     1				IVIS, VIS, MOSFILE, CVMFILE
      LOGICAL		DATEXIST, FILEXIST, DOTV
      LOGICAL		DOSTOP, FINISH, CNVRGE, T, F, FIRST
      REAL              TBEG, TEND, TMIN, TMAX
      COMMON		/MOSCOM/	TBEG, TEND
      EXTERNAL		MOSCLRES
      CHARACTER*(SYSMXNAM)	STRM2, STRTIMC
      DATA		T	/.TRUE./
      DATA		F	/.FALSE./
      DATA		BLC	/SYSMXDIM*1/
      DATA		TRC	/SYSMXDIM*1/
C==================================================================
      CALL MSGWELCO ('I perform maximum entropy mosaicing of the SUN')
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
      CALL USRGETC ('Vis', VISFILE, MAXNPC, NDUMMY)
      CALL USRGETC ('Mosaic', MOSFILE, 1, NDUMMY)
      CALL USRGETC ('Default', DEFFILE, 1, NDUMMY)
      CALL USRGETC ('VM', VMFILE, 1, NDUMMY)
      CALL USRGETC ('CVM', CVMFILE, 1, NDUMMY)
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETR ('Tflux', TFLUX, 1, NDUMMY)
      CALL USRGETR ('Sigma', SIGMA, MAXNPC, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETR ('Tolerance', TOLER, 1, NDUMMY)
      CALL USRGETR ('Npoints', Q, 1, NDUMMY)
      CALL USRGETR ('Gain', TOL, 1, NDUMMY)
      CALL USRGETL ('Dotv', DOTV, 1, NDUMMY)
      CALL USRGETR ('Solrad', SOLRAD, 1, NDUMMY)
      CALL USRGETR ('Psun', PSUN, 1, NDUMMY)
      CALL USRGETR ('Tref', TREF, 1, NDUMMY)
      CALL USRGETR ('Timestep', TSTEP, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Process input parameters
C
      VMLIM = 5
      DOSTOP = NITER.LT.0.0
      IF (NITER.NE.0) VMLIM = IABS(NITER)
C
C Subclass to be gridded: can only deconvolve Stokes I
C
      SUBCLASS = 'OBS/I'
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
      DO 3 IPC = 1, MAXNPC
         IF(VISFILE(IPC).NE.' ') THEN
            NPC = NPC + 1
            CALL MSGPUT ('Reading standard visibility file', 'I')
            IVIS = 'IM/PC'//STRINT(NPC)
            CALL VISGET (IVIS, VISFILE(IPC), 'I', '*', ' ')
         END IF
 3    CONTINUE
      CALL DATPUTI ('IM', 'NPC', NPC, 1)
      IF(NPC.EQ.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'You must specify an input file via Vis and/or Mosaic')
         GO TO 999
      ELSE
         WRITE (MESSAGE, 1050) NPC
 1050    FORMAT ('There are ',I4,' pointings in all')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Now select data
C
      TBEG = 1E20
      TEND = -1E20
      CALL UTLTTOD (TIMR(1), TIME(1))
      CALL UTLTTOD (TIMR(5), TIME(2))
      CALL DATCREAT('M')
      CALL DATPUTI ('M', 'NPC', NPC, 1)
      CALL DATPUTR ('M', 'SIGMA', SIGMA, MAXNPC)
      DO 1 IPC = 1, NPC
         IVIS = 'IM/PC'//STRINT(IPC)
         VIS = 'M/PC'//STRINT(IPC)
         CALL VISATF (IVIS)
         CALL VISSEL (IVIS, SUBCLASS, TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) IPC, NSEL
 1000    FORMAT ('Pointing ',I4,' : selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
         CALL VISSUB (IVIS, 'OBS', 'I', VIS)
         CALL DATDELET (IVIS)
         CALL ARRCOPY (STRM2(VIS,'OBS/I/WT'), STRM2(VIS,
     1      'OBS/I/OLDWT'))
         CALL ARRSTAT (STRM2(VIS,'TIME'), ' ')
         CALL DATGETR (STRM2(VIS,'TIME'), 'ARRMIN', TMIN, 1, NDUMMY)
         CALL DATGETR (STRM2(VIS,'TIME'), 'ARRMAX', TMAX, 1, NDUMMY)
         TBEG = MIN(TMIN, TBEG)
         TEND = MAX(TMAX, TEND)
         TRANGE(1) = TMIN
         TRANGE(2) = TMAX
         CALL DATPUTR (VIS, 'TRANGE', TRANGE, 2)
   1  CONTINUE
      CALL DATDELET('IM')
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got data: '//CTIME, 'I')
      MESSAGE = 'Start of data = '//STRTIMC(TBEG)
      CALL MSGPUT (MESSAGE, 'I')
      MESSAGE = 'End of data   = '//STRTIMC(TEND)
      CALL MSGPUT (MESSAGE, 'I')
      IF (TREF.EQ.0.0) THEN
         TREF = 0.5 * (TBEG + TEND)
         MESSAGE = 'Reference time set to '//STRTIMC(TREF)
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         TREF = TREF / 24
         MESSAGE = 'Reference time specified as '//STRTIMC(TREF)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Get images. First try to get existing VM image. If it does not
C exist then create it.
C
      IF (FILEXIST(VMFILE)) THEN
         CALL FILIMGGE ('VM', VMFILE, ' ')
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
         IF (TFLUX.NE.0.0) THEN
            CALL ARRSETCO ('VM', ABS(TFLUX), 0.0)
         ELSE
            CALL ARRSETCO ('VM', 0.0, 1.0)
         END IF
         IF (ERROR) GO TO 999
         CALL MSGPUT ('Initial guess is a flat image', 'I')
      END IF
C
C Find dual image: Pad by factor of two e.g. 60 -> 128
C
      PIMSIZE(1) = FFTPWR2(IMSIZE(1))
      PIMSIZE(2) = FFTPWR2(IMSIZE(2))
      PIMSIZE(3) = 1
      CALL IMGMAKE (STRM2('M/PC1', SUBCLASS), CELLSIZE, PIMSIZE, 
     1   SHIFT, 'R', 'PSF')
      CALL FFTCONJA ('PSF', 'VMVis', DIR, 0)
C
C Make some visibility files
C
      DO 4 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL VISCLONE (VIS, 'OBS', 'I', 'MOD')
         IF (ERROR) GO TO 999
  4   CONTINUE
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
      IF (FOV.NE.0.0) THEN
         CALL DATPUTR ('VMVis', 'WTFOV', FOV, 1)
         DO 2 IPC = 1, NPC
            VIS = 'M/PC'//STRINT(IPC)
            CALL GRDUWT (VIS, 'OBS/I', 'VMVis')
            CALL ARRCOPY (STRM2(VIS, 'OBS/I/WT'), 
     1         STRM2(VIS, 'MOD/I/WT'))
            IF (ERROR) GO TO 999
  2      CONTINUE
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Uniform weighting: '//CTIME, 'I')
      END IF
C
C Apply taper
C
      IF ((TAPER(1).NE.0.0).OR.(TAPER(2).NE.0.0)) THEN
         DO 6 IPC = 1, NPC
            VIS = 'M/PC'//STRINT(IPC)
            CALL VISTAPER (VIS, SUBCLASS, TAPER, SUBCLASS)
            CALL ARRCOPY (STRM2(VIS, 'OBS/I/WT'), 
     1         STRM2(VIS, 'MOD/I/WT'))
            IF (ERROR) GO TO 999
 6          CONTINUE
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
      CALL DATPUTR ('VM', 'SOLRAD', SOLRAD, 1)
      CALL DATPUTR ('VM', 'PSUN', PSUN, 1)
      CALL DATPUTR ('VM', 'TREF', TREF, 1)
      CALL DATPUTR ('VM', 'TSTEP', TSTEP/24, 1)
      CALL DATPUTR ('TmpVM', 'SOLRAD', SOLRAD, 1)
      CALL DATPUTR ('TmpVM', 'PSUN', PSUN, 1)
      CALL DATPUTR ('TmpVM', 'TREF', TREF, 1)
      CALL DATPUTR ('TmpVM', 'TSTEP', TSTEP/24, 1)
C
C Find image size
C
      CALL DATGETAR ('VM', NAX, NAXIS, ATYPE, AADD)
      IF (ERROR) GO TO 999
      IMSZ = 1
      DO 10 IAX = 1, NAX
         IMSZ = IMSZ * MAX(1, NAXIS(IAX))
  10  CONTINUE
C
C Now take care of default image
C
      IF (FILEXIST(DEFFILE)) THEN
         CALL FILIMGGE ('Default', DEFFILE, ' ')
         DEFLEV = 0.0
         CALL MSGPUT ('Using specified image as default', 'I')
      ELSE
         IF (TFLUX.NE.0.0) THEN
            DEFLEV = ABS(TFLUX)/IMSZ
         ELSE
            DEFLEV = 1.0
         END IF
         CALL MSGPUT ('Using flat default', 'I')
      END IF
      IF (ERROR) GO TO 999
C
C Initial CHISQ
C
      SRVAR = 0.0
      IF(SIGMA(1).EQ.0.0) SIGMA(1) = 0.001
      DO 15 IPC = 1,  NPC
         IF(SIGMA(IPC).EQ.0.0) SIGMA(IPC) = SIGMA(1)
         SRVAR = SRVAR + 1.0/SIGMA(IPC)**2
 15   CONTINUE
      SRVAR = SRVAR/NPC
      TCHISQ = NPC * IMSZ / (Q * SRVAR)
      CALL DATPUTR ('Residual', 'Q', Q, 1)
      CALL MOSCLRES ('VM', 'Residual', CHISQ)
      NOISE = SQRT(CHISQ/TCHISQ)
      WRITE (MESSAGE, 1300) NOISE
 1300 FORMAT ('Initial fit = ',F10.3)
      CALL MSGPUT (MESSAGE, 'I')
C
C ************************ Perform iterations ***********************
C
      ITER = BITER 
      CALL MSGPUT (
     1   'Iteration      Entropy     Flux        Fit     Gradient',
     2   'I')
      FINISH = F
      CNVRGE = F
  200 CONTINUE
      ITER = ITER + 1
      IF (ITER.GE.VMLIM) FINISH = T
      CALL DATPUTR ('Residual', 'Q', Q, 1)
      TCHISQ = NPC * IMSZ / (Q * SRVAR)
      CALL MEMONEIT (ITER.EQ.1, 'VM', 'Default', DEFLEV, 'Step', 
     1   'Residual', Q, TFLUX, TCHISQ, TOL, FLUX, CHISQ, ENTRPY, 
     2   NRMGRD, MOSCLRES)
      IF (ERROR) GO TO 999
C
C Print out current information
C
      NOISE = SQRT(CHISQ/TCHISQ)
      WRITE (MESSAGE, 1400) ITER, ENTRPY, FLUX, NOISE, NRMGRD
 1400 FORMAT (1X,I5,5X,4(F10.3,1X))
      CALL MSGPUT (MESSAGE, 'I')
      IF (DOTV.AND.(.NOT.ERROR)) THEN
         CALL IMGTVD ('VM', 0.0, 0.0)
         IF(ERROR) THEN
            CALL MSGPUT ('Error writing TV file', 'W')
            CALL ERRCANCE
         ENDIF
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
            CALL USRGETR ('Tflux', TFLUX, 1, NDUMMY)
         END IF
      END IF
C                              Check if it's time to stop
      CNVRGE = (NOISE.LT.(1.0+TOLER)) .AND. 
     1    (ITER.NE.(BITER+1)) .AND. (NRMGRD.LT.TOLER)
      IF (TFLUX.GT.0.0) CNVRGE =  CNVRGE .AND. 
     1    (ABS(FLUX-TFLUX).LT.TOLER*TFLUX)
      IF (CNVRGE) THEN
         IF (DOSTOP) THEN
            FINISH = T
         ELSE 
            TOL = NRMGRD
         END IF
      END IF
      IF (.NOT.FINISH) GO TO 200
C
C ************************ End of Iteration ************************
C
C Write answer
C
      CALL DATPUTI ('VM', 'NITER', ITER, 1)
      CALL HISOPEN ('VM')
      CALL HISINPUT ('VM')
      CALL HISPUT ('VM', 
     1   'Iteration      Entropy     Flux        Fit     Gradient')
      WRITE (MESSAGE, 1400) ITER, ENTRPY, FLUX, NOISE, NRMGRD
      CALL HISPUT ('VM', MESSAGE)
C
C Now write output image
C
      IF (VMFILE.NE.' ' ) THEN         
         CALL DATGETD ('VM', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
         CALL DATPUTD ('VM', 'OBSRA', RVAL(1), 1)
         CALL DATPUTD ('VM', 'OBSDEC', RVAL(2), 1)
         CALL DATPUTC ('VM', 'BUNIT', 'JY/PIXEL', 1)
         CALL FILIMGPU ('VM', VMFILE, ' ')
      END IF
C
C Restore image if required
C
      IF (CVMFILE.NE.' ') THEN
         CALL MOSCALRR ('VM', 'Residual')
         CALL IMGCLONE ('VM', 'CVM')
         CALL CRDGET ('VM', NAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         IF (ERROR) GOTO 999
         SBEAM(1) = BEAM(1)/ABS(3600.0*DELT(1))
         SBEAM(2) = BEAM(2)/ABS(3600.0*DELT(2))
         SBEAM(3) = BEAM(3)
         SBEAM(4) = 0.0
         CALL IMGSMOOT ('VM', SBEAM, 'CVM', 'VMVis')
         CALL DATPUTC ('CVM', 'BUNIT', 'JY/BEAM', 1)
         CALL DATPUTR ('CVM', 'BMAJ', BEAM(1)/3600.0, 1)
         CALL DATPUTR ('CVM', 'BMIN', BEAM(2)/3600.0, 1)
         CALL DATPUTR ('CVM', 'BPA',  BEAM(3), 1)
         CALL DATPUTR ('CVM', 'BZ', BEAM(4)/3600.0, 1)
         CALL DATGETD ('CVM', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
         CALL DATPUTD ('CVM', 'OBSRA', RVAL(1), 1)
         CALL DATPUTD ('CVM', 'OBSDEC', RVAL(2), 1)
         FACT = SQRT(ATAN(1.0)/LOG(2.0))
         BFACT = FACT**2 * SBEAM(1) * SBEAM(2) 
         CALL ARRLC ('CVM', BFACT, 'Residual', 1.0, 'CVM')
         CALL FILIMGPU ('CVM', CVMFILE, ' ')
      END IF
C
 999  CONTINUE
      END
C
      SUBROUTINE MOSCLRES (MODEL, RES, CHISQ)
C
CD Calculate Chi-squared and grad Chi-squared for visibility
CD data. This routine is called by VM routines. WORKS in
CD uv plane or image plane as directed
C
CS Arguments: CALL MOSCLRES (MODEL, RES, CHISQ)
CS
CS	MODEL	CH*(*)	input	Name of model image
CS	CHISQ	REAL	output	Chi-squared
CS	RES	CH*(*)	input	Name of grad Chi-squared image
CA Audit trail:
CA	Original version: Audit trail comments go on this line
CA	and successive lines
CA				T.J.Cornwell	Jan 5 1989
CE
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
      INTEGER		IAX, NAX, NAXIS (SYSMXDIM), NDUMMY, VSADD,
     1			WTADD, DATADD, IPC, NPC, NELEMENT, AADD, NSEL
      REAL		DBCHISQ, Q, RMS, SIGMA(MAXNPC), SRVAR, WT
      DOUBLE PRECISION	OBSRA, OBSDEC
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM) 	STRM2, TELESCOP, VIS
      CHARACTER*6	STRINT
      LOGICAL		DATEXIST, FIRST
      REAL              TBEG, TEND, PSUN, TREF, TSTEP, TIME(2),
     $                  UVLIMITS(2), PHI, TRANGE(2)
      COMMON		/MOSCOM/ TBEG, TEND
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
      DATA UVLIMITS/0.0,1E10/
C==================================================================
      CHISQ = 0.0
      CALL DATGETR (RES, 'Q', Q, 1, NDUMMY) 
C
      IF (ERROR) GO TO 999 
C
      CALL DATGETR (MODEL, 'PSUN', PSUN, 1, NDUMMY)
      CALL DATGETR (MODEL, 'TREF', TREF, 1, NDUMMY) 
      CALL DATGETR (MODEL, 'TSTEP', TSTEP, 1, NDUMMY)
C 
C Find residuals
C
      CALL DATGETI ('M', 'NPC', NPC, 1, NDUMMY)
      CALL DATGETR ('M', 'SIGMA', SIGMA, MAXNPC, NDUMMY)
      SRVAR = 0.0
      DO 1 IPC = 1, NPC
         SRVAR = SRVAR + 1.0/SIGMA(IPC)**2
 1    CONTINUE
      SRVAR = SRVAR/NPC
C
C Loop over all times
C
      FIRST = .TRUE.
      TIME(1) = TBEG
      TIME(2) = TIME(1) + TSTEP
 2    CONTINUE
      DO 10 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL DATGETR (VIS, 'TRANGE', TRANGE, 2, NDUMMY)
         IF ((TRANGE(2).LT.TIME(1)).OR.(TRANGE(1).GT.TIME(2))) GOTO 10
         WT = 1 / (SRVAR *SIGMA(IPC)**2)
         CALL DATGETD (VIS, 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATPUTD (MODEL, 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('TmpVM', 'OBSRA', OBSRA, 1)
         CALL DATGETD (VIS, 'OBSDEC', OBSDEC, 1, NDUMMY)
         CALL DATPUTD (MODEL, 'OBSDEC', OBSDEC, 1)
         CALL DATPUTD ('TmpVM', 'OBSDEC', OBSDEC, 1)
         CALL DATGETC (VIS, 'TELESCOP', TELESCOP, 1, NDUMMY)
         CALL DATPUTC (MODEL, 'TELESCOP', TELESCOP, 1)
         CALL DATPUTC ('TmpVM', 'TELESCOP', TELESCOP, 1)
         IF (PSUN.NE.0.0) THEN
            PHI =  2 * PI * (0.5*(TIME(1)+TIME(2)) - TREF)/PSUN
            CALL IMGSOLRO (MODEL, 'TmpVM', PHI)
            CALL IMGPB ('TmpVM', 'TmpVM', 'APPLY')
         ELSE
            CALL IMGPB (MODEL, 'TmpVM', 'APPLY')
         END IF
         CALL IMGGRIDC ('TmpVM', 'TmpVM', 'CORRECT')
         CALL IMGFFT ('TmpVM', 'VMVis')
         CALL VISDEGRI (VIS, 'MOD/I', 'VMVis')
         CALL ARRSUBTR (STRM2(VIS, 'OBS/I/VIS'), 
     1      STRM2(VIS, 'MOD/I/VIS'), STRM2(VIS, 'MOD/I/VIS'))
         CALL VISGRID (VIS, 'MOD/I', 'VMVis', .FALSE.)
         CALL IMGFFT ('VMVis', 'TmpVM')
         CALL IMGGRIDC ('TmpVM', 'TmpVM', 'CORRECT')
         CALL ARRRMS ('TmpVM', RMS, DBCHISQ)
         CALL IMGPB ('TmpVM', 'TmpVM', 'APPLY')
         CALL ARRSCALE ('TmpVM', WT, 0.0, 'TmpVM')
C
C Rotate back
C
         IF (FIRST) THEN
            CALL IMGSOLRO ('TmpVM', RES, -PHI)
            CHISQ = WT * DBCHISQ / Q
            FIRST = .FALSE.
         ELSE
            CALL IMGSOLRO  ('TmpVM', 'TmpVM', -PHI)
            CALL ARRADD (RES, 'TmpVM', RES)
            CHISQ = CHISQ + WT * DBCHISQ / Q
         END IF
  10  CONTINUE
      TIME(1) = TIME(2)
      TIME(2) = TIME(2) + TSTEP
      IF(TIME(2).LE.TEND) GO TO 2
C
      IF (CHISQ.LE.0.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Chi-squared zero')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
C
      SUBROUTINE MOSCALRR (MODEL, RES)
C
CD Calculate Residual corrected for primary beam
C
CS Arguments: CALL MOSCALRR (MODEL, RES)
CS
CS	MODEL	CH*(*)	input	Name of model image
CS	RES	CH*(*)	input	Name of grad Chi-squared image
CA Audit trail:
CA	Original version: Audit trail comments go on this line
CA	and successive lines
CA				T.J.Cornwell	Jan 5 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, RES
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSCALRR')
C
      INTEGER		IAX, NAX, NAXIS (SYSMXDIM), NDUMMY, VSADD,
     1			WTADD, DATADD, IPC, NPC, NELEMENT, AADD, NSEL
      REAL		CHISQ, Q, RMS, DBCHISQ, SIGMA(MAXNPC), SRVAR,
     $                  WT
      DOUBLE PRECISION	OBSRA, OBSDEC
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM) 	STRM2, TELESCOP, VIS
      CHARACTER*6	STRINT
      LOGICAL		DATEXIST, FIRST
      REAL              TBEG, TEND
      COMMON		/MOSCOM/ TBEG, TEND
      REAL		PSUN, TREF, TSTEP, PHI, TIME(2), UVLIMITS(2)
      REAL              TRANGE(2)
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
      DATA              UVLIMITS/0.0,1E10/
C==================================================================
      CALL DATGETR (RES, 'Q', Q, 1, NDUMMY)
C
      IF (ERROR) GO TO 999
C
C
      CALL DATGETR (MODEL, 'TSTEP', TSTEP, 1, NDUMMY)
      CALL DATGETR (MODEL, 'PSUN', PSUN, 1, NDUMMY)
      CALL DATGETR (MODEL, 'TREF', TREF, 1, NDUMMY)
C
      CALL IMGCLONE (MODEL, 'Flat')
      CALL IMGCLONE (MODEL, 'PBFlat')
      CALL IMGCLONE (MODEL, 'Denom')
      CALL ARRSETCO ('Flat', 0.0, 1.0)
C
C Find residuals
C
      CALL DATGETR ('M', 'SIGMA', SIGMA, MAXNPC, NDUMMY)
      CALL DATGETI ('M', 'NPC', NPC, 1, NDUMMY)
      SRVAR = 0.0
      DO 1 IPC = 1,  NPC
         SRVAR = SRVAR + 1.0/SIGMA(IPC)**2
 1    CONTINUE
      SRVAR = SRVAR/NPC
C
C Loop over all times
C
      FIRST = .TRUE.
      TIME(1) = TBEG
      TIME(2) = TIME(1) + TSTEP
 2    CONTINUE
      DO 10 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL DATGETR (VIS, 'TRANGE', TRANGE, 2, NDUMMY)
         IF ((TRANGE(2).LT.TIME(1)).OR.(TRANGE(1).GT.TIME(2))) GOTO 10
         WT = 1 / (SRVAR *SIGMA(IPC)**2)
         CALL DATGETD (VIS, 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATPUTD (MODEL, 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('TmpVM', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Flat', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Denom', 'OBSRA', OBSRA, 1)
         CALL DATGETD (VIS, 'OBSDEC', OBSDEC, 1, NDUMMY)
         CALL DATPUTD (MODEL, 'OBSDEC', OBSDEC, 1)
         CALL DATPUTD ('TmpVM', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTD ('Flat', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTD ('Denom', 'OBSDEC', OBSDEC, 1)
         CALL DATGETC (VIS, 'TELESCOP', TELESCOP, 1, NDUMMY)
         CALL DATPUTC (MODEL, 'TELESCOP', TELESCOP, 1)
         CALL DATPUTC ('TmpVM', 'TELESCOP', TELESCOP, 1)
         CALL DATPUTC ('Flat', 'TELESCOP', TELESCOP, 1)
         CALL DATPUTC ('Denom', 'TELESCOP', TELESCOP, 1)
         CALL ARRCOPY (STRM2(VIS,'OBS/I/OLDWT'), STRM2(VIS,
     1      'OBS/I/WT'))
         CALL VISSEL(VIS, 'OBS/I', TIME, UVLIMITS, NSEL)
         IF (PSUN.NE.0.0) THEN
            PHI =  2 * PI * (0.5*(TIME(1)+TIME(2)) - TREF)/PSUN
            CALL IMGSOLRO (MODEL, 'TmpVM', PHI)
            CALL IMGPB ('TmpVM', 'TmpVM', 'APPLY')
         ELSE
            CALL IMGPB (MODEL, 'TmpVM', 'APPLY')
         END IF
         CALL IMGGRIDC ('TmpVM', 'TmpVM', 'CORRECT')
         CALL IMGFFT ('TmpVM', 'VMVis')
         CALL VISDEGRI (VIS, 'MOD/I', 'VMVis')
         CALL ARRSUBTR (STRM2(VIS, 'OBS/I/VIS'), 
     1      STRM2(VIS, 'MOD/I/VIS'), STRM2(VIS, 'MOD/I/VIS'))
         CALL VISGRID (VIS, 'MOD/I', 'VMVis', .FALSE.)
         CALL IMGFFT ('VMVis', 'TmpVM')
         CALL IMGGRIDC ('TmpVM', 'TmpVM', 'CORRECT')
         CALL ARRSCALE ('TmpVM', WT, 0.0, 'TmpVM')
C
C Accumulate as required
C
         IF (FIRST) THEN
            CALL IMGPB ('TmpVM', RES, 'APPLY')
            CALL IMGPB ('Flat', 'Denom', 'APPLY')
            CALL IMGPB ('Denom', 'Denom', 'APPLY')
            FIRST = .FALSE.
         ELSE
            CALL IMGPB ('TmpVM', 'TmpVM', 'APPLY')
            CALL ARRADD (RES, 'TmpVM', RES)
            CALL IMGPB ('Flat', 'PBFlat', 'APPLY')
            CALL IMGPB ('PBFlat', 'PBFlat', 'APPLY')
            CALL ARRADD ('Denom', 'PBFlat', 'Denom')
         END IF
  10  CONTINUE
      TIME(1) = TIME(2)
      TIME(2) = TIME(2) + TSTEP
      IF(TIME(2).LE.TEND) GO TO 2
C
C Now take ratio
C
      CALL ARRCLIP ('Denom', 1E-2*NPC, 1E10, 'Denom')
      CALL ARRDIV (RES, 'Denom', RES)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C
      SUBROUTINE IMGSOLRO (IN, OUT, PHI)
C
C Rotate SUN through small angle
C
C
C	IN	CH*(*)	input	Name of directory entry
C	OUT	CH*(*)	input	Name of directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	IN, OUT
      REAL		PHI
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSOLRO')
C
      CHARACTER*(SYSMXNAM)	TELESCOP
      INTEGER		NDUMMY, I, STRSEARC
      INTEGER		NAX, NAXIS(SYSMXDIM)
      INTEGER		RNAX, CRDRNAX
      INTEGER		INADD, OUTADD, DATADD
      REAL		CX, CY, FWHM, BMX, BMY
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CDELT(SYSMXDIM), RAD
      DOUBLE PRECISION	FREQ, CRVAL(SYSMXDIM), DTMP
      CHARACTER*1	TYPE
      REAL		C(SYSMXDIM), PC(SYSMXDIM)
      DATA	C	/SYSMXDIM*0.0/
      DATA	PC	/SYSMXDIM*0.0/
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, NAX, NAXIS, TYPE, INADD)
      OUTADD = DATADD (OUT)
      RNAX = CRDRNAX(NAX, NAXIS)
C
      CALL DATGETD (IN, 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
      CALL DATGETC (IN, 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
C
      IF (ERROR) GO TO 990
      CALL DATGETD (IN, 'SOLRA', DTMP, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         PC(1) = CRVAL(1)
         CALL DATPUTD (IN, 'SOLRA', CRVAL(1), 1)
      ELSE
         PC(1) = DTMP
      END IF
      CALL DATGETD (IN, 'SOLDEC', DTMP, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         PC(2) = CRVAL(2)
         CALL DATPUTD (IN, 'SOLDEC', CRVAL(2), 1)
      ELSE
         PC(2) = DTMP
      END IF
      CALL CRDWTOP (IN, PC, C)
C
      CALL DATGETR (IN, 'SOLRAD', RAD, 1, NDUMMY)
      CALL DATGETR (IN, 'CDELT', CDELT, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 990
      RAD = ABS(RAD/(3600.0*CDELT(1)))
      IF (RNAX.EQ.2) THEN
         CALL PIX2DSOL (MEMR(INADD), NAXIS(1), NAXIS(2), 
     1      C(1), C(2), RAD, PHI, MEMR(OUTADD))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Can only treat 2D images')
         GO TO 999
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
      SUBROUTINE PIX2DSLR (IN, NX, NY, CX, CY, RAD, DPH, OUT)
C
C Introduce small rotation to spherical source model
C
C      IN     REAL(*) input   Array to be manipulated
C      NX     INT     input   Number pixels, X axis
C      NY     INT     input   Number pixels, Y axis
C      CX     REAL    input   X position disk center
C      CY     REAL    input   Y position disk center
C      RAD    REAL    input   Apparent radius of disk
C      DPHI   REAL    input   Rotation thru DPH degrees longitude
C      OUT    REAL(*) output  Output model
C Audit trail:
C      Changed to pass input image where the disk is not defined.
C                                      T.J. Cornwell 1 June 1989
C      Changed interpolation scheme.   T.S. Bastian 25 July 1989
C
C----------------------------------------------------------------------
#include       "stdinc.h"
      INTEGER          NX, NY, II, IX, IY, NFRST
      INTEGER          ITAB(3), IOP(4), ROW, NVP, IGOOD
      REAL             IN(NX, *), OUT(NX, *)
      REAL             CX, CY, Z, RAD, DPH, TMP
      REAL             RELX, RELY, PI, THETA, DELPHI
      REAL             S1BUF(1024), S2BUF(1024)
      REAL             S1YY(1024), S2YY(1024)
      REAL             X(1024), PHI(1024), PGRID(1024)
      REAL             WKF(4096), WKB(4096), TAB(3)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DSOL')
C
      DATA             PI /3.1415926/, ITAB /1,0,0/, IOP /4,4,0,0/
C======================================================================
      IF (ERROR) GO TO 999
C                                       Loop over NY
      DO 900 IY = 1, NY
         NVP = 0
C                                       Accumulate pixels for 
C                                       interpolation; set those 
C                                       beyond "limb" to original value
         RELY = FLOAT(IY) - CY
         DO II = 1, NX
            OUT(II, IY) = IN(II, IY)
            RELX = FLOAT(II) - CX
            IF (SQRT(RELY**2+RELX**2).LE.RAD) THEN
               NVP = NVP + 1
               IF (NVP.EQ.1) NFRST = II
               S1BUF(NVP) = IN(II, IY)
               X(NVP) = RELX
               OUT(II, IY) = IN(II, IY)
            END IF
         END DO
         IF (NVP.LT.4) GO TO 900
C
         THETA = ASIN(RELY/RAD)
         Z = RAD*COS(THETA)
         DO II = 1, NVP
            TMP = 1.0 - (X(II)/Z)**2
            IF (TMP.GT.0.0) THEN
               PHI(II) = SIGN(ACOS(SQRT(TMP)),X(II))
            ELSE
               PHI(II) = SIGN(ACOS(0.0),X(II))
            END IF
            S1YY(II) = 0.0
         END DO 
         DELPHI = PI/(NX-1)
         DO II = 1, NX
            PGRID(II) = -PI/2.+(II-1)*DELPHI
            S2YY(II) = 0.0
         END DO
C                                       Set up forward interpolation.
         CALL COEFF1 (NVP, PHI, S1BUF, S1YY, IOP, 1, WKF)
C                                       Interpolate onto PGRID and
C                                       rotate.
         DO IX = 1, NX
            IF (PGRID(IX).LT.PHI(1) .OR. PGRID(IX).GT.PHI(NVP)) THEN
               S2BUF(IX) = 0.0
            ELSE
               CALL TERP1 (NVP, PHI, S1BUF, S1YY, PGRID(IX), 1,  
     1            TAB, ITAB)
               S2BUF(IX) = TAB(1)
            END IF
            PGRID(IX) = PGRID(IX) + DPH
         END DO
C                                       Reverse interpolation.
         CALL COEFF1 (NX, PGRID, S2BUF, S2YY, IOP, 1, WKB)
C
         DO IX = 1, NVP
            IF (PHI(IX).GT.PGRID(1) .AND. PHI(IX).LT.PGRID(NX)) THEN
               CALL TERP1 (NX, PGRID, S2BUF, S2YY, PHI(IX), 1, 
     1            TAB, ITAB)
               OUT(NFRST+IX-1, IY) = TAB(1)
            END IF
         END DO
C
C
 900  CONTINUE
C
 999  RETURN
      END
C
