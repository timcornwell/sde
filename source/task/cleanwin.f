C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)cleanwin.f	1.7    11/18/94
C
      SUBROUTINE SDEMAIN
C
CD Program to remake CLEAN and residual images with different windows
C
C	This does all convolutions to the full size of the PSF, regardless
C	of the actual size of the window.  Slower, but better aliasing
C	performance.
C
C PSFBEAM is always fitted to (or obtained from) the PSF, and is the
C appropriate beam to attach to the residuals.  This is also attached to the
C components, as it weakly influences the placement of the components, if not
C their flux scale.  The output image will carry the restoring beam, RESBEAM,
C either obtained in the normal way from the parameters, or the pseudo-Gaussian
C parameters fit to the restoring beam and scaled to keep the fluxes correct.
C
C Audit trail:
C	Cloned from clean.f
C				D.S.Briggs	Feb 21 1992
C	Write out padded component image if desired
C				D.S.Briggs	Nov 4 1992
C       Allow arbitrary Restoring PSF, and write out "smoothed" component
C	image if desired.
C				D.S.Briggs	Feb 4 1993
C	Allow rescaling of the residuals
C				D.S.Briggs	Apr 15 1993
C	Write the bloody task inputs to the output files!
C				D.S.Briggs	Dec 8 1993
C	Write pseudo-Gaussian beam parameters to output image when using
C	an arbitrary restoring beam, so that subsequent tasks can understand
C	the flux scale.  Mass renaming of BEAM variables to make more
C	sense.  FitAlg parameter added.
C				D.S.Briggs	Aug 2 1994
C	Support for Smooth Pixel CLEANs added
C				D.S.Briggs	Aug 14 1994
C	Coordinate information was corrupted on SmoComp and Clean output!
C				D.S.Briggs	Nov 18 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CLEANWIN')
C
      INTEGER		NDUMMY,
     2			BLC (SYSMXDIM), TRC (SYSMXDIM),
     3			STEP (SYSMXDIM)
      REAL		PSFBEAM(4), RESBEAM(4), PIXBEAM(4)
      LOGICAL		DOFULL, DORESCAL, DORESMO, DOSMOPIX
      CHARACTER*(SYSMXNAM)	CLNFILE, RESFILE, DRTFILE, PSFFILE,
     $			SCLNFILE, OCLNFILE, CLNTYPE, RSTFILE,
     $			SCMPFILE, RAWRESFL, TEMP, FITALG, SPSFFILE
      CHARACTER		ATYPE*1
      INTEGER		NAX, NAXIS(SYSMXDIM), I
      REAL		RPEAK, BPPBG, BPPBR, SPSFMAX, RSPSFMAX
C
      DATA		STEP	/SYSMXDIM*1/, PSFBEAM/4*0.0/
C
      REAL		DATFGETR
C==================================================================
      CALL MSGWELCO ('I rewindow deconvolutions')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
      CALL USRGETC ('RawResid', RAWRESFL, 1, NDUMMY)
      CALL USRGETC ('Components', CLNFILE, 1, NDUMMY)
      CALL USRGETC ('OutComponents', OCLNFILE, 1, NDUMMY)
      CALL USRGETC ('SmoComponents', SCMPFILE, 1, NDUMMY)
      CALL USRGETC ('CLEAN', SCLNFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', RESBEAM, 4, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETC ('RBeam', RSTFILE, 1, NDUMMY)
      CALL USRGETL ('Rescale', DORESCAL, 1, NDUMMY)
      CALL USRGETL ('Resmooth', DORESMO, 1, NDUMMY)
      CALL USRGETL ('SmoPix', DOSMOPIX, 1, NDUMMY)
      CALL USRGETR ('PixBeam', PIXBEAM, 4, NDUMMY)
      CALL USRGETC ('SmoPSF', SPSFFILE, 1, NDUMMY)
      CALL USRGETL ('Full', DOFULL, 1, NDUMMY)
      CALL USRGETC ('Algorithm', TEMP, 1, NDUMMY)
      CALL STRUC (TEMP, CLNTYPE)
      IF (ERROR) GO TO 999
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Algorithm type?  (Used only to figure out what the windows were)
C
      IF (CLNTYPE.EQ.'CLARK') THEN
         CALL MSGPUT ('Clark algorithm', 'I')
      ELSE IF (CLNTYPE.EQ.'SDI') THEN
         CALL MSGPUT ('Steer-Dewdney-Ito algorithm','I')
      ELSE IF (CLNTYPE.EQ.'HOGBOM') THEN
         CALL MSGPUT ('Hogbom algorithm', 'I')
      ELSE IF (CLNTYPE.EQ.'VM') THEN
         CALL MSGPUT ('VM algorithm', 'I')
      ELSE IF (CLNTYPE.EQ.' ') THEN
         CLNTYPE = 'CLARK'
         CALL MSGPUT ('Something else.  (treated like Clark)', 'I')
      ELSE
         CALL ERRREPOR (ERRBADID, ROUTINE, 'CLEAN type not recognized')
         GO TO 999
      END IF
C
C Get images
C
      CALL FILIMGGE ('InitialDirty', DRTFILE, ' ')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
      CALL FILIMGGE ('Components', CLNFILE, ' ')
      IF (ERROR) GO TO 999
      IF (RSTFILE.NE.' ') THEN
         CALL FILIMGGE ('RestBeam', RSTFILE, ' ')
         IF (ERROR) GO TO 999
         CALL ARRSTAT ('RestBeam', ' ')
         RPEAK = DATFGETR ('RestBeam', 'ARRMAX')
         IF (ABS(RPEAK-1.0).GT.1.E-5) THEN
            WRITE (MESSAGE, 1000) RPEAK
 1000       FORMAT ('Warning: Peak of restoring beam is',E13.5,
     $         ' -- Rescaling')
            CALL ARRSCALE ('RestBeam', 1/RPEAK, 0.0, 'RestBeam')
            CALL MSGPUT (MESSAGE,'W')
         END IF
      END IF
C
C Beam stuff
C
      CALL MSGPUT ('PSF Fitted Beam:','I')
      CALL FILBEMGE ('PSF', PSFBEAM, FITALG, 'BEAM')
      CALL MSGPUT ('Fitted Restoring Beam:','I')
      IF (RSTFILE.EQ.' ') THEN
         CALL FILBEMGE ('PSF', RESBEAM, FITALG, 'BEAM')
      ELSE
C
C Figure out an "effective" Gaussian restoring beam for arbitrary beams.
C
         CALL IMGBMSHP ('RestBeam')
         CALL HEDBEMGE ('RestBeam', RESBEAM, ' ', -1.0)
         CALL IMGP2PB ('RestBeam', RESBEAM, ' ')
         BPPBG = DATFGETR ('RestBeam', 'BPPB')
         CALL ARRSTAT ('RestBeam', ' ')
         BPPBR = DATFGETR ('RestBeam', 'ARRSUM')
         RESBEAM(1) = RESBEAM(1) * SQRT(BPPBR / BPPBG)
         RESBEAM(2) = RESBEAM(2) * SQRT(BPPBR / BPPBG)
         CALL MSGPUT ('Effective Restoring Beam:','I')
         WRITE (MESSAGE, 1010) RESBEAM(1), RESBEAM(2), RESBEAM(3)
 1010    FORMAT ('Fit Beam (FWHM in sec) = ',2F12.5,F7.1)
         CALL MSGPUT (MESSAGE,'I')
      END IF
C
C Figure out an appropriate window
C
      CALL DATGETAR ('InitialDirty', NAX, NAXIS, ATYPE, NDUMMY)
      IF (DOFULL) THEN
         DO 100 I = 1, SYSMXDIM
            BLC(I) = MAX(1, MIN(BLC(I), NAXIS(I)))
            TRC(I) = MAX(1, MIN(TRC(I), NAXIS(I)))
            IF (TRC(I).EQ.1) TRC(I) = MAX(1, NAXIS(I))
 100     CONTINUE
      ELSE
         IF (CLNTYPE.EQ.'HOGBOM') THEN
            CALL CRDNHALF ('InitialDirty', BLC, TRC)
         ELSE IF ((CLNTYPE.EQ.'CLARK').OR.(CLNTYPE.EQ.'SDI')
     $            .OR.(CLNTYPE.NE.' ')) THEN
            CALL CRDHALF ('InitialDirty', BLC, TRC)
         END IF
      END IF
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
      CALL IMGSUBSE ('InitialDirty', 'Dirty', 'Window')
      CALL DATDELET ('InitialDirty')
      CALL IMGCLONE ('Dirty', 'Residual')
      IF (ERROR) GO TO 999
C
C Force image(s) to full grid
C
      CALL DATRENAM ('Components','RawComponents')
      CALL IMGFITTO ('RawComponents', 'PSF', 'Components')
      CALL HEDCOPY ('RawComponents', 'Components')
      CALL HEDCOPY ('PSF', 'Components')
      CALL DATDELET ('RawComponents')
C
      IF (RSTFILE.NE.' ') THEN
         CALL DATRENAM ('RestBeam', 'RawRestBeam')
         CALL IMGFITTO ('RawRestBeam', 'PSF', 'RestBeam')
         CALL HEDCOPY ('RawRestBeam', 'RestBeam')
         CALL HEDCOPY ('PSF', 'RestBeam')
         CALL DATDELET ('RawRestBeam')
      END IF
C
C Mess with the pixel function
C
      IF (DOSMOPIX) THEN
         CALL MSGPUT ('Using Smooth Pixel Function','I')
         CALL FILBEMGE ('PSF', PIXBEAM, FITALG, 'BEAM')
         IF (SPSFFILE.NE.' ') THEN
            CALL MSGPUT ('Calculating PSF scaling','I')
            CALL FILIMGGE ('SmoPSF', SPSFFILE, ' ')
            CALL DATPUTC ('PSF', 'FFTSIZE', 'EXACT', 1)
            CALL IMGSMOOT ('PSF', PIXBEAM, 'RawSmoPSF',
     $         'SmoSwork')
            CALL ARRSTAT ('SmoPSF', ' ')
            SPSFMAX = DATFGETR ('SmoPSF','ARRMAX')
            CALL ARRSTAT ('RawSmoPSF', ' ')
            RSPSFMAX = DATFGETR ('RawSmoPSF','ARRMAX')
            CALL ARRSCALE ('Components', SPSFMAX/RSPSFMAX, 0.0,
     $         'Components')
            WRITE (MESSAGE,1100) SPSFMAX/RSPSFMAX
 1100       FORMAT ('Scaling components by ',F10.6)
            CALL MSGPUT (MESSAGE,'I')
         END IF
         CALL DATRENAM ('Components', 'CompCoef')
         CALL DATPUTC ('CompCoef', 'FFTSIZE', 'EXACT', 1)
         CALL IMGSMOOT ('CompCoef', PIXBEAM, 'Components', 'CoefWork')
         CALL HEDCOPY ('CompCoef', 'Components')
      END IF
C
C Write padded components if needed
C
      IF (OCLNFILE.NE.' ') THEN
         CALL IMGFITTO ('Components', 'Dirty', 'OutComponents')
         CALL HEDCOPY ('Components', 'OutComponents')
         CALL HEDCOPY ('Dirty', 'OutComponents')
         CALL HISINPUT ('OutComponents')
         CALL FILIMGPU ('OutComponents', OCLNFILE, ' ')
         CALL DATDELET ('OutComponents')
      END IF
C
C Recalculate residual image to full window
C
      CALL IMGCONV0 ('Components', 'PSF', 'XFR', 'BigModel', 'MVis')
      CALL IMGFITTO ('BigModel', 'Dirty', 'Model')
      CALL HEDCOPY ('BigModel', 'Model')
      CALL HEDCOPY ('Dirty', 'Model')
      CALL DATDELET ('BigModel')
      CALL ARRSUBTR ('Dirty', 'Model', 'Residual')
C
C Rescale them if needed
C
      IF (DORESCAL) THEN
         IF (RAWRESFL.NE.' ') THEN
            CALL HEDBEMPU ('Residual', PSFBEAM, 'JY/BEAM', 0.0)
            CALL HISINPUT ('Residual')
            CALL FILIMGPU ('Residual', RAWRESFL, ' ')
         END IF
         CALL DATPUTC ('Residual', 'FFTSIZE', 'EXACT', 1)
         IF (DORESMO) THEN
            CALL MSGPUT ('Resmoothing residuals','I')
            CALL IMGSMOOT ('Residual', RESBEAM, 'SmoRes', 'XImage')
            CALL DATDELET ('Residual')
            CALL IMGUNSMO ('SmoRes', PSFBEAM, 'Residual', 'XImage')
c            call filimgpu ('SmoRes', 'SMORES', ' ')
c            call filimgpu ('Residual', 'UNSMORES', ' ')
            CALL DATDELET ('SmoRes')
         END IF
         CALL IMGPB2P ('Residual', PSFBEAM, 'Residual')
         CALL IMGP2PB ('Residual', RESBEAM, 'Residual')
      END IF
C
      IF (RESFILE.NE.' ') THEN
         IF (DORESCAL) THEN
            CALL HEDBEMPU ('Residual', RESBEAM, 'JY/BEAM', 0.0)
         ELSE
            CALL HEDBEMPU ('Residual', PSFBEAM, 'JY/BEAM', 0.0)
         END IF
         CALL HISINPUT ('Residual')
         CALL FILIMGPU ('Residual', RESFILE, ' ')
      END IF
C
C Now do 'conventional' CLEAN image if required
C
      IF ((SCLNFILE.NE.' ').OR.(SCMPFILE.NE.' ')) THEN
         IF (RSTFILE.NE.' ') THEN
            CALL IMGCONV0 ('Components', 'RestBeam', 'RestXFR',
     $           'BigSmoComp', 'MVis')
            CALL HEDBEMPU ('BigSmoComp', RESBEAM, ' ', BPPBR)
         ELSE
            CALL IMGSMOOT ('Components', RESBEAM, 'BigSmoComp', 'MVis')
            CALL IMGP2PB ('BigSmoComp', RESBEAM, 'BigSmoComp')
         END IF
         CALL IMGFITTO ('BigSmoComp', 'Dirty', 'SmoComp')
         CALL HEDCOPY ('BigSmoComp', 'SmoComp')
         CALL CRDCOPY ('Dirty', 'SmoComp')
         CALL HEDACOPY ('Dirty', 'SmoComp')
         CALL DATDELET ('BigSmoComp')
         IF (SCMPFILE.NE.' ') THEN
            CALL DATPUTC ('SmoComp', 'BUNIT', 'JY/BEAM', 1)
            CALL HISINPUT ('SmoComp')
            CALL FILIMGPU ('SmoComp', SCMPFILE, ' ')
         END IF
      END IF
C
      IF (SCLNFILE.NE.' ') THEN
         CALL IMGCLONE ('SmoComp', 'Clean')
         CALL ARRLC ('SmoComp', 1.0, 'Residual', 1.0, 'Clean')
         CALL DATPUTC ('Clean', 'BUNIT', 'JY/BEAM', 1)
         CALL HISINPUT ('Clean')
         CALL FILIMGPU ('Clean', SCLNFILE, ' ')
      END IF
C
 999  CONTINUE
      END
