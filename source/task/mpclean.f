C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mpclean.f	1.1    11/21/94
C
      SUBROUTINE SDEMAIN
C
CD Program to perform Multi PSF CLEAN deconvolution
C
C Audit trail:
C	Cloned from clean.f
C				D.S.Briggs	Nov 20 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MPCLEAN')
C
      INTEGER		MAXPSF
      PARAMETER		(MAXPSF = 30)
C
      INTEGER		NITER, ANITER, NDUMMY, I,NPSF, NBOX,
     $			BLC (SYSMXDIM), TRC (SYSMXDIM),
     $			BBBLC(SYSMXDIM), BBTRC(SYSMXDIM)
      REAL		FLUX, GAIN, MAXRES, BEAM(4)
      CHARACTER*(SYSMXNAM)	CLNFILE, RESFILE, DRTFILE,
     $   		PSFFILE(MAXPSF), SCLNFILE, CTIME, STRINT,
     $   		CCFILE, PBOXFILE(MAXPSF-1), TEMP,
     $   		FITALG, BOXFILE
C
      CHARACTER*(SYSMXNAM) STRINT
      LOGICAL		FILEXIST, DATEXIST
C==================================================================
      CALL MSGWELCO ('I perform Multi PSF CLEAN deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, MAXPSF, NDUMMY)
      CALL USRGETC ('PBox', PBOXFILE, MAXPSF-1, NDUMMY)
      CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
      CALL USRGETC ('Components', CLNFILE, 1, NDUMMY)
      CALL USRGETC ('Box', BOXFILE, 1, NDUMMY)
      CALL USRGETC ('CCfile', CCFILE, 1, NDUMMY)
      CALL USRGETC ('CLEAN', SCLNFILE, 1, NDUMMY)
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETR ('Flux', FLUX, 1, NDUMMY)
      CALL USRGETR ('Gain', GAIN, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Get PSFs and BOX
C
      CALL FILIMGGE ('InitialDirty', DRTFILE, ' ')
      NPSF = 0
 10   CONTINUE
      IF ((NPSF.LT.MAXPSF).AND.(PSFFILE(NPSF+1).NE.' ')) THEN
         NPSF = NPSF + 1
         TEMP = 'PSF' // STRINT(NPSF)
         CALL FILIMGGE (TEMP, PSFFILE(NPSF), ' ')
         IF (ERROR) GO TO 999
         GO TO 10
      END IF
C
      IF (BOXFILE.NE.' ')
     $   CALL FILMASGE ('Box', BOXFILE, 'InitialDirty')
      IF (ERROR) GO TO 999
C
C Fit a Gaussian Beam to PSF
C
      CALL FILBEMGE ('PSF'//STRINT(NPSF), BEAM, FITALG, 'BEAM')
      IF (ERROR) GO TO 999
C
C Contract Window if requested
C
      IF ((BLC(1).LT.0).OR.(TRC(1).LT.0)) THEN
         IF (BOXFILE.EQ.' ') THEN
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
         WRITE (MESSAGE, 1010) TRC(1), TRC(2), TRC(3)
 1010    FORMAT (' TRC = ',I4,',',I4,',',I4)
         CALL MSGPUT(MESSAGE,'I')
      END IF
C
C Cut out inner portion of Dirty Image
C
      CALL CRDNHALF ('InitialDirty', BLC, TRC)
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
      CALL IMGSUBSE ('InitialDirty', 'Dirty', 'Window')
      CALL DATDELET ('InitialDirty')
      CALL IMGCLONE ('Dirty', 'Residual')
      IF (ERROR) GO TO 999
C
C Now get the PSF Boxes
C
      NBOX = 0
 20   CONTINUE
      IF ((NBOX.LT.MAXPSF-1).AND.(PBOXFILE(NBOX+1).NE.' ')) THEN
         NBOX = NBOX + 1
         TEMP = 'PBox' // STRINT(NBOX)
         CALL FILMASGE (TEMP, PBOXFILE(NBOX), 'Dirty')
         IF (ERROR) GO TO 999
         GO TO 20
      END IF
C
      IF (NBOX.NE.NPSF-1) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE,
     $      'There must be one less PBox than PSF')
         GO TO 999
      END IF
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got input: '//CTIME, 'I')
C
C Basic housekeeping
C
      IF (BOXFILE.NE.' ') THEN
         CALL DATRENAM ('Box', 'RawBox')
         CALL IMGFITTO ('RawBox', 'Dirty', 'Box')
         CALL DATDELET ('RawBox')
      ELSE
         CALL IMGCLONE('Dirty', 'Box')
         CALL ARRSETCO('Box', 0.0, 1.0)
      END IF
C
C Stack the PSFs and PBoxs into 3-D cubes.
C
      DO 100 I = 1, NBOX
         CALL DATRENAM ('PBox'//STRINT(I), 'RawPBox')
         CALL IMGFITTO ('RawPBox', 'Dirty', 'PBox'//STRINT(I))
         CALL DATDELET ('RawPBox')
 100  CONTINUE
      CALL ARRSTACK ('PSF#', 'PSFSTK', 1, NPSF, 1, .TRUE.)
      IF (NBOX.GT.1) THEN
         CALL ARRSTACK ('PBox#', 'PBoxSTK', 1, NBOX, 1, .TRUE.)
      ELSE
         CALL DATRENAM ('PBox1', 'PBoxSTK')
      END IF
C
      CALL FILSYSEX (CLNFILE, TEMP)
      IF(FILEXIST(CLNFILE).AND.(TEMP.EQ.'SDE')) THEN
         CALL MSGPUT ('Sorry -- I don''t do restarts','I')
      END IF
C
      CALL IMGCLONE ('Dirty', 'Components')
      CALL ARRSETCO ('Components', 0.0, 0.0)
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
      CALL IMGMPCLN ('Dirty', 'PSFSTK', 'PBoxSTK', 'Components', 
     $   'Residual', 'Box')
      CALL DATGETI ('Components', 'NITER', ANITER, 1, NDUMMY)
      CALL DATGETR ('Components', 'FLUX', MAXRES, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
      WRITE (MESSAGE, 1000) ANITER, MAXRES
 1000 FORMAT ('Subtracted ',I6,' components: maximum residual ',
     1  1PE12.4,' Jy/beam')
      CALL HISPUT ('Components', MESSAGE)
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
