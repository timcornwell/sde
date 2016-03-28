C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)clean.f	1.8    11/7/94
C
      SUBROUTINE SDEMAIN
C
CD Program to perform the CLEAN algorithm of deconvolution
C
C Audit trail:
C	Hogbom or Clark CLEAN capability
C				T.J. Cornwell	August 23 1989
C       Added Clean Boxes 
C                               R.G. Marson     Dec 20 1990 
C       Clean boxes tweaked, SDI Clean added.  If speed ever becomes an
C       issue, we can probably gain 20% or so by passing a flag
C       to bypass the boxes if they are not needed.
C				D.S.Briggs	Feb 25 1992
C	Non-negative cleaning added
C				D.S.Briggs	Mar 2 1993
C	And tweaked to a slightly different algorithm
C				D.S.Briggs	Mar 20 1993
C	Added selction of BEAMFIT algorithm based on value of BEAM(1)
C				D.S.Briggs	Oct 7 1993
C	Revamped beam selection to use FILBEMGE
C				D.S.Briggs	June 14 1994
C	Added smooth pixel clean.  (Gaussian Pixel Function)
C				D.S.Briggs	Aug 14 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CLEAN')
C
      INTEGER		NITER, BITER, ANITER, NDUMMY, CCNL, BP,
     $			BLC (SYSMXDIM), TRC (SYSMXDIM),
     $			BBBLC(SYSMXDIM), BBTRC(SYSMXDIM), DIR
      REAL		FLUX, GAIN, MAXRES, BEAM(4), TRIM,
     $   		PIXBEAM(4)
      CHARACTER*(SYSMXNAM)	CLNFILE, RESFILE, DRTFILE, PSFFILE,
     $			SCLNFILE, CTIME, STRINT, CCFILE, CLNTYPE,
     $                  BOXFILE, TEMP, FITALG
      LOGICAL		DOSMOPIX
C
      LOGICAL		FILEXIST, DATEXIST
C==================================================================
      CALL MSGWELCO ('I perform CLEAN deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
      CALL USRGETC ('Components', CLNFILE, 1, NDUMMY)
      CALL USRGETC ('Box', BOXFILE, 1, NDUMMY)
      CALL USRGETC ('CCfile', CCFILE, 1, NDUMMY)
      CALL USRGETC ('CLEAN', SCLNFILE, 1, NDUMMY)
      CALL USRGETC ('Algorithm', TEMP, 1, NDUMMY)
      CALL STRUC (TEMP, CLNTYPE)
      IF (ERROR) GO TO 999
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETR ('Flux', FLUX, 1, NDUMMY)
      CALL USRGETR ('Gain', GAIN, 1, NDUMMY)
      CALL USRGETR ('Trim', TRIM, 1, NDUMMY)
      CALL USRGETI ('Numcl', CCNL, 1, NDUMMY)
      CALL USRGETI ('Bpatch', BP, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETL ('SmoPix', DOSMOPIX, 1, NDUMMY)
      CALL USRGETR ('PixBeam', PIXBEAM, 4, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Algorithm type?
C
      IF (CLNTYPE.EQ.' ') CLNTYPE = 'CLARK'
      IF (CLNTYPE.EQ.'CLARK') THEN
         CALL MSGPUT ('Clark algorithm', 'I')
      ELSE IF (CLNTYPE.EQ.'SDI') THEN
         CALL MSGPUT ('Steer-Dewdney-Ito algorithm','I')
      ELSE IF (CLNTYPE.EQ.'HOGBOM') THEN
         CALL MSGPUT ('Hogbom algorithm', 'I')
      ELSE IF (CLNTYPE.EQ.'NN') THEN
         CALL MSGPUT ('Non-negative Hogbom algorithm', 'I')
      ELSE IF (CLNTYPE.EQ.'NN1') THEN
         CALL MSGPUT ('Non-negative Hogbom algorithm (old style)','I')
      ELSE
         CALL ERRREPOR (ERRBADID, ROUTINE, 'CLEAN type not recognized')
         GO TO 999
      END IF
C
C Get images
C
      CALL FILIMGGE ('InitialDirty', DRTFILE, ' ')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
      IF (BOXFILE.NE.' ')
     $   CALL FILMASGE ('Box', BOXFILE, 'InitialDirty')
      IF (ERROR) GO TO 999
C
C Fit a Gaussian Beam to PSF
C
      CALL FILBEMGE ('PSF', BEAM, FITALG, 'BEAM')
C
C Smooth the raw beam if requested
C
      IF (DOSMOPIX) THEN
         CALL MSGPUT ('Smooth Pixels','I')
         CALL FILBEMGE ('PSF', PIXBEAM, FITALG, 'BEAM')
         CALL DATRENAM ('PSF','OriginalPSF')
         CALL DATPUTC ('PSF', 'FFTSIZE', 'EXACT', 1)
         CALL IMGSMOOT ('OriginalPSF', PIXBEAM, 'PSF', 'SmoothWork')
         CALL HEDACOPY ('OriginalPSF', 'PSF')
         CALL DATDELET ('SmoothWork')
      END IF
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
      IF (CLNTYPE.EQ.'CLARK') THEN
         CALL CRDHALF ('InitialDirty', BLC, TRC)
      ELSE
         CALL CRDNHALF ('InitialDirty', BLC, TRC)
      END IF
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
      CALL IMGSUBSE ('InitialDirty', 'Dirty', 'Window')
      CALL DATDELET ('InitialDirty')
      CALL IMGCLONE ('Dirty', 'Residual')
C
C Now make transfer function, etc.
C
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL FFTCONJA ('PSF', 'MVis', DIR, 0)
      IF(CLNTYPE.EQ.'CLARK') THEN
         CALL IMGSUBPS ('PSF', 'SPSF', BP)
         CALL DATDELET ('PSF')
      END IF
      IF (BOXFILE.NE.' ') THEN
         CALL DATRENAM ('Box', 'RawBox')
         CALL IMGFITTO ('RawBox', 'Dirty', 'Box')
         CALL DATDELET ('RawBox')
      ELSE
         CALL IMGCLONE('Dirty', 'Box')
         CALL ARRSETCO('Box', 0.0, 1.0)
      END IF
      CALL FILSYSEX (CLNFILE, TEMP)
      IF(FILEXIST(CLNFILE).AND.(TEMP.EQ.'SDE')) THEN
         CALL FILIMGGE ('Components', CLNFILE, ' ')
         CALL DATGETI ('Components', 'NITER', BITER, 1, NDUMMY)
         CALL DATPUTI ('Components', 'BITER', BITER+1, 1)
         CALL MSGPUT (
     1      'Restarting CLEAN from iteration '//STRINT(BITER), 'I')
         CALL IMGRESID ('Components', 'Dirty', 'XFR', 'Residual', 
     1      'MVis')
         CALL ARRCOPY ('Residual', 'Dirty')
      ELSE
         CALL IMGCLONE ('Dirty', 'Components')
         CALL ARRSETCO ('Components', 0.0, 0.0)
      END IF
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
      IF (CLNTYPE.EQ.'CLARK') THEN
         CALL DATPUTI ('Components', 'CCNL', CCNL, 1)
         CALL IMGCCLEA ('Dirty', 'SPSF', 'XFR', 'Components', 
     1      'Residual', 'Box', 'MVis')
      ELSE IF (CLNTYPE.EQ.'HOGBOM') THEN
         CALL IMGCLEAN ('Dirty', 'PSF', 'Components', 
     $        'Residual', 'Box')
      ELSE IF (CLNTYPE.EQ.'NN') THEN
         CALL IMGNNCLN ('Dirty', 'PSF', 'Components', 
     $        'Residual', 'Box')
      ELSE IF (CLNTYPE.EQ.'NN1') THEN
         CALL IMGNNCL1 ('Dirty', 'PSF', 'Components', 
     $        'Residual', 'Box')
      ELSE IF (CLNTYPE.EQ.'SDI') THEN
         CALL DATPUTR ('Components', 'TRIM', TRIM, 1)
         CALL IMGSDICL ('Dirty', 'XFR', 'Components',
     $      'Residual', 'Box', 'MVis')
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
C Deal with smooth pixels
C
      IF (DOSMOPIX) THEN
         CALL MSGPUT ('Smoothing Components','I')
         CALL DATRENAM ('Components', 'RawComponents')
         CALL DATPUTC ('RawComponents', 'FFTSIZE', 'EXACT', 1)
         CALL IMGSMOOT ('RawComponents', PIXBEAM, 'Components',
     $      'SmoothWork')
         CALL HEDACOPY ('RawComponents', 'Components')
         CALL IMGMAKEX ('OriginalPSF', 'XFR')
      END IF
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
