C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)svdclean.f	1.1    6/8/94
C
      SUBROUTINE SDEMAIN
C
CD Program to perform SVD CLEAN deconvolution
C
C Audit trail:
C
C	Cloned from clean.f
C				D.S.Briggs	3 Dec 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SVDCLEAN')
C
      INTEGER		NITER, ANITER, NDUMMY, CCNL, BP, NPIX(2),
     $			BLC (SYSMXDIM), TRC (SYSMXDIM), DIR
      REAL		FLUX, GAIN, MAXRES, BEAM(4), DATFGETR
      CHARACTER*(SYSMXNAM)	CLNFILE, RESFILE, DRTFILE, PSFFILE,
     $			SCLNFILE, CTIME, CCFILE,
     $                  BOXFILE, INBXFILE, ALGORITHM, TEMP, INBOX
C
      LOGICAL		DATEXIST
C==================================================================
      CALL MSGWELCO ('I perform SVD CLEAN deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
      CALL USRGETC ('Components', CLNFILE, 1, NDUMMY)
      CALL USRGETC ('Box', BOXFILE, 1, NDUMMY)
      CALL USRGETC ('InBox', INBXFILE, 1, NDUMMY)
      CALL USRGETC ('CCfile', CCFILE, 1, NDUMMY)
      CALL USRGETC ('CLEAN', SCLNFILE, 1, NDUMMY)
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
      CALL USRGETI ('Bpatch', BP, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Algorithm type?
C
      IF (ALGORITHM.EQ.'QUICK') THEN
         CALL MSGPUT ('Will use fixed beam patch','I')
      ELSE IF (ALGORITHM.EQ.'SLOW') THEN
         CALL MSGPUT ('Will compute beam patch as needed','I')
      ELSE IF (ALGORITHM.EQ.'HIST') THEN
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
      CALL FILMASGE ('Box', BOXFILE, 'InitialDirty')
      IF (ALGORITHM.EQ.'HIST') THEN
         IF (INBXFILE.EQ.' ') THEN
            INBOX = 'Box'
         ELSE
            INBOX = 'InBox'
            CALL FILMASGE (INBOX, INBXFILE, 'InitialDirty')
         END IF
      END IF
      IF (ERROR) GO TO 999
C
C Cut out appropriate chunk of Dirty Image
C
      CALL CRDHALF ('InitialDirty', BLC, TRC)
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
      CALL IMGSUBSE ('InitialDirty', 'Dirty', 'Window')
      CALL DATDELET ('InitialDirty')
      CALL IMGCLONE ('Dirty', 'Residual')
C
C Fit a Gaussian Beam to PSF
C
      IF (BEAM(1) .EQ. 0. ) THEN
         CALL MSGPUT ('Be darned careful with auto-fit beams!','W')
         CALL IMGBMSHP( 'PSF' )
         IF (ERROR) GO TO 999
         BEAM(1) =  DATFGETR( 'PSF', 'BMAJ')*3600.0
         BEAM(2) =  DATFGETR( 'PSF', 'BMIN')*3600.0
         BEAM(3) =  DATFGETR( 'PSF', 'BPA')
         BEAM(4) =  DATFGETR( 'PSF', 'BZ')*3600.0
      ENDIF
C
C Now make transfer function, etc.
C
      CALL DATPUTC ('PSF', 'FFTSIZE', 'EXACT', 1)
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL FFTCONJA ('PSF', 'MVis', DIR, 0)
C
      CALL DATRENAM ('Box', 'RawBox')
      CALL IMGFITTO ('RawBox', 'Dirty', 'Box')
      CALL DATDELET ('RawBox')
C
      IF (DATEXIST('InBox')) THEN
         CALL DATRENAM ('InBox', 'RawInBox')
         CALL IMGFITTO ('RawInBox', 'Dirty', 'InBox')
         CALL DATDELET ('RawInBox')
      END IF
C
      CALL IMGCLONE ('Dirty', 'Components')
      CALL ARRSETCO ('Components', 0.0, 0.0)
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
      CALL DATPUTI ('Components', 'CCNL', CCNL, 1)
      CALL DATPUTI ('Components', 'BP', BP, 1)
      CALL DATPUTI ('Components', 'NPIX', NPIX, 2)
      IF (ALGORITHM.EQ.'QUICK') THEN
         CALL IMGSVDCL ('Dirty', 'PSF', 'XFR', 'Components', 
     1      'Residual', 'Box', 'MVis')
      ELSE IF (ALGORITHM.EQ.'SLOW') THEN
         CALL IMGSVDC1 ('Dirty', 'PSF', 'XFR', 'Components', 
     1      'Residual', 'Box', 'MVis')
      ELSE IF (ALGORITHM.EQ.'HIST') THEN
         CALL IMGSVDC2 ('Dirty', 'PSF', 'XFR', 'Components', 
     1      'Residual', 'Box', INBOX, 'MVis')
      ELSE
         CALL MSGPUT ('NYI','E')
         GO TO 999
      END IF
      CALL DATGETI ('Components', 'NITER', ANITER, 1, NDUMMY)
      CALL DATGETR ('Components', 'FLUX', MAXRES, 1, NDUMMY)
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
      CALL MSGPUT ('Finished CLEAN: '//CTIME, 'I')
C
C Write answer: First do Components file
C
      IF (CLNFILE.NE.' ') THEN
         CALL DATPUTI ('Components', 'NITER', ANITER, 1)
         CALL DATPUTC ('Components', 'BUNIT', 'JY/PIXEL', 1)
         CALL FILIMGPU ('Components', CLNFILE, ' ')
      END IF
C
C Write CC file if required
C
      IF (CCFILE.NE.' ') THEN
         CALL MSGPUT ('Writing Clean Component List','I')

         IF (CLNFILE.NE.' ') THEN
            WRITE (STRBUF, 2010) CLNFILE
         ELSE
            WRITE (STRBUF, 2010) SCLNFILE
         END IF
 2010    FORMAT ('/ CC list for image ',A)
         CALL IMGCLIS ('Components', CCFILE, STRBUF)
      END IF
C
C Now do conventional CLEAN image if required
C
      IF (SCLNFILE.NE.' ') THEN
         CALL IMGCLONE ('Components', 'Clean')
         CALL IMGSMOOT ('Components', BEAM, 'Clean', 'MVis')
         CALL IMGP2PB ('Clean', BEAM, 'Clean')
         CALL ARRLC ('Clean', 1.0, 'Residual', 1.0, 'Clean')
         CALL DATPUTC ('Clean', 'BUNIT', 'JY/BEAM', 1)
         CALL DATPUTI ('Components', 'NITER', ANITER, 1)
         CALL FILIMGPU ('Clean', SCLNFILE, ' ')
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
