C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)inls.f	1.1    7/13/94
C
      SUBROUTINE SDEMAIN
C
CD Program to perform iterative NNLS deconvolution
C
C Audit trail:
C       Initial version
C				D.S.Briggs	June 18 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'INLS')
C
      INTEGER		NITER, ANITER, NDUMMY, NPIX(2),
     $			BLC (SYSMXDIM), TRC (SYSMXDIM), DIR
      REAL		FLUX, GAIN, MAXRES, BEAM(4)
      CHARACTER*(SYSMXNAM)	IMGFILE, RESFILE, DRTFILE, PSFFILE,
     $			CMPFILE, CTIME, DATWFILE, FLXWFILE, SLCTFILE,
     $			MODFILE, FITALG, ALGORITHM, TEMP, INITSLCT
C==================================================================
      CALL MSGWELCO ('I perform Iterative NNLS deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC ('InitialSelect', SLCTFILE, 1, NDUMMY)
      CALL USRGETC ('DataWindow', DATWFILE, 1, NDUMMY)
      CALL USRGETC ('FluxWindow', FLXWFILE, 1, NDUMMY)
      CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
      CALL USRGETC ('Components', CMPFILE, 1, NDUMMY)
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETC ('Algorithm', TEMP, 1, NDUMMY)
      CALL STRUC (TEMP, ALGORITHM)
      IF (ERROR) GO TO 999
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETI ('Npix', NPIX, 2, NDUMMY)
      CALL USRGETR ('Flux', FLUX, 1, NDUMMY)
      CALL USRGETR ('Gain', GAIN, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Algorithm type?
C
      IF (ALGORITHM.EQ.'HIST') THEN
         CALL MSGPUT ('Will compute beam patch via histogram','I')
      ELSE
         CALL MSGPUT ('Unrecognized algorithm '//ALGORITHM,'E')
         GO TO 999
      END IF
C
C Get images
C
      CALL FILIMGGE ('InitialDirty', DRTFILE, ' ')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
C
      CALL FILMASGE ('DataWindow', DATWFILE, 'InitialDirty')
      CALL FILMASGE ('FluxWindow', FLXWFILE, 'InitialDirty')
C
      IF (MODFILE.NE.' ') CALL FILIMGGE ('Model', MODFILE, ' ')
      INITSLCT = ' '
      IF (SLCTFILE.NE.' ') THEN
         INITSLCT = 'InitialSelect'
         CALL FILIMGGE (INITSLCT, SLCTFILE, ' ')
      END IF
      IF (ERROR) GO TO 999
C
C Cut out appropriate chunk of dirty image
C
      CALL CRDHALF ('InitialDirty', BLC, TRC)
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
      CALL IMGSUBSE ('InitialDirty', 'Dirty', 'Window')
      CALL DATDELET ('InitialDirty')
      CALL IMGCLONE ('Dirty', 'Residual')
      IF (ERROR) GO TO 999
C
C Fit a Gaussian Beam to PSF
C
      CALL FILBEMGE ('PSF', BEAM, FITALG, 'BEAM')
      IF (ERROR) GO TO 999
C
C Make sure all coordinate systems are consistent
C
      CALL DATRENAM ('DataWindow', 'RawWindow')
      CALL IMGFITTO ('RawWindow', 'Dirty', 'DataWindow')
      CALL DATDELET ('RawWindow')
      CALL DATRENAM ('FluxWindow', 'RawWindow')
      CALL IMGFITTO ('RawWindow', 'Dirty', 'FluxWindow')
      CALL DATDELET ('RawWindow')
C
      IF (MODFILE.EQ.' ') THEN
         CALL IMGCLONE ('Dirty', 'Components')
         CALL ARRSETCO ('Components', 0.0, 0.0)
      ELSE
         CALL IMGFITTO ('Model', 'Dirty', 'Components')
         CALL DATDELET ('Model')
      END IF
C
      IF (INITSLCT.NE.' ') THEN
         CALL DATRENAM (INITSLCT, 'RawInitSelect')
         CALL IMGFITTO ('RawInitSelect', 'Dirty', INITSLCT)
         CALL DATDELET ('RawInitSelect')
      END IF
C
C Now make transfer function, etc.
C
      CALL DATPUTC ('PSF', 'FFTSIZE', 'EXACT', 1)
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL FFTCONJA ('PSF', 'MVis', DIR, 0)
C
      IF (NPIX(2).LE.0) NPIX(2) = NPIX(1)
C
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got images: '//CTIME, 'I')
C
C Add history cards
C
      CALL HISOPEN ('Components')
      CALL HISINPUT ('Components')
C
C Call main routine
C
      CALL DATPUTI ('Components', 'NITER', NITER, 1)
      CALL DATPUTR ('Components', 'FLUX', FLUX, 1)
      CALL DATPUTR ('Components', 'GAIN', GAIN, 1)
      CALL DATPUTI ('Components', 'NPIX', NPIX, 2)
      IF (ALGORITHM.EQ.'HIST') THEN
         CALL IMGINLS ('Dirty', 'PSF', 'XFR', 'Components',
     $      'Residual', 'DataWindow', 'FluxWindow', 'MVis',
     $      INITSLCT, ALGORITHM)
      ELSE
         CALL MSGPUT ('NYI','E')
         GO TO 999
      END IF
      IF (ERROR) GO TO 999
C
      WRITE (MESSAGE, 1000) ANITER, MAXRES
 1000 FORMAT ('Subtracted ',I6,' components: maximum residual ',
     1  1PE12.4,' Jy/beam')
      CALL HISPUT ('Components', MESSAGE)
C
      CALL IMGRESID ('Components', 'Dirty', 'XFR', 'Residual', 
     1   'MVis')
C
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Finished INNLS: '//CTIME, 'I')
C
C Write answer: First do Components file
C
      IF (CMPFILE.NE.' ') THEN
         CALL DATPUTI ('Components', 'NITER', ANITER, 1)
         CALL DATPUTC ('Components', 'BUNIT', 'JY/PIXEL', 1)
         CALL FILIMGPU ('Components', CMPFILE, ' ')
      END IF
C
C Now do conventional CLEAN image if required
C
      IF (IMGFILE.NE.' ') THEN
         CALL IMGCLONE ('Components', 'Image')
         CALL IMGSMOOT ('Components', BEAM, 'Image', 'MVis')
         CALL IMGP2PB ('Image', BEAM, 'Image')
         CALL ARRLC ('Image', 1.0, 'Residual', 1.0, 'Image')
         CALL DATPUTC ('Image', 'BUNIT', 'JY/BEAM', 1)
         CALL DATPUTI ('Components', 'NITER', ANITER, 1)
         CALL FILIMGPU ('Image', IMGFILE, ' ')
      END IF
C
C Residual image
C
      IF (RESFILE.NE.' ') THEN
         CALL DATPUTC ('Residual', 'BUNIT', 'JY/BEAM', 1)
         CALL FILIMGPU ('Residual', RESFILE, ' ')
      END IF
C
 999  CONTINUE
      END
