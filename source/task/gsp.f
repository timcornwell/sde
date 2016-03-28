C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gsp.f	1.6    11/30/93
C
      SUBROUTINE SDEMAIN
C
CD Program to perform the GSP algorithm of deconvolution
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added beam fitting
C				T.J. Cornwell	Sept 13 1989
C	Added 'Clean boxes'.  Fixed minor residual bug.
C				D.S.Briggs	March 5 1992
C	Added smarter windowing behavior, and fixed the restart
C       capability.
C				D.S.Briggs	Feb 23 1993
C	Changed units of GSP image to JY/PIXEL
C				D.S.Briggs	Nov 23 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GSP')
C
      INTEGER		NITER, BITER, ANITER, NDUMMY,
     2			BLC (SYSMXDIM), TRC (SYSMXDIM),
     3			STEP (SYSMXDIM), I, NAX, NAXIS(SYSMXDIM)
      REAL		FLUX, GAIN, MAXRES, MINRES,
     $   		BEAM(4), ACCEL, DATFGETR
      LOGICAL		FILEXIST
      CHARACTER		ATYPE*1
      CHARACTER*(SYSMXNAM)	GSPFILE, RESFILE, DRTFILE, PSFFILE,
     1			SGSPFILE, CTIME, STRINT, BOXFILE, TEMP
C
      DATA		STEP	/SYSMXDIM*1/
C==================================================================
      CALL MSGWELCO ('I perform GSP deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
      CALL USRGETC ('Components', GSPFILE, 1, NDUMMY)
      CALL USRGETC ('GSP', SGSPFILE, 1, NDUMMY)
      CALL USRGETC ('Box', BOXFILE, 1, NDUMMY)
      IF (ERROR) GO TO 999
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETR ('Flux', FLUX, 1, NDUMMY)
      CALL USRGETR ('Gain', GAIN, 1, NDUMMY)
      CALL USRGETR ('Accel', ACCEL, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Get the main images
C
      CALL FILIMGGE ('Initial Dirty', DRTFILE, ' ')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
C
C Figure out a reasonable window
C
      CALL DATGETAR ('Initial Dirty', NAX, NAXIS, ATYPE, NDUMMY)
      DO 10 I = 1, SYSMXDIM
         IF (BLC(I).LE.0) BLC(I) = 1
         BLC(I) = MIN(BLC(I),NAXIS(I))
         IF (TRC(I).LE.0) TRC(I) = NAXIS(I)
         TRC(I) = MIN(TRC(I),NAXIS(I))
 10   CONTINUE
      WRITE (MESSAGE, 1010) BLC
 1010 FORMAT (' BLC =',6(I4,','),I4)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1020) TRC
 1020 FORMAT (' TRC =',6(I4,','),I4)
      CALL MSGPUT (MESSAGE, 'I')
C
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
C
C Subsection as needed
C
      CALL IMGSUBSE ('Initial Dirty', 'Dirty', 'Window')
      CALL DATDELET ('Initial Dirty')
      CALL IMGCLONE ('Dirty', 'Residual')
C
C And a mask
C
      IF (BOXFILE.NE.' ') THEN
         CALL FILMASGE ('Box', BOXFILE, 'Dirty')
      ELSE
         CALL IMGCLONE('Dirty', 'Box')
         CALL ARRSETCO('Box', 0.0, 1.0)
      END IF
C
C Fit a Gaussian Beam to PSF
C
      IF (BEAM(1) .EQ. 0. ) THEN
         CALL IMGBMSHP( 'PSF' )
         IF (ERROR) GO TO 999
         BEAM(1) =  DATFGETR( 'PSF', 'BMAJ')*3600.0
         BEAM(2) =  DATFGETR( 'PSF', 'BMIN')*3600.0
         BEAM(3) =  DATFGETR( 'PSF', 'BPA')
         BEAM(4) =  DATFGETR( 'PSF', 'BZ')*3600.0
      ENDIF
C
C Now make transfer functions, etc.
C
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL DATDELET ('PSF')
      CALL IMGCLONE ('XFR', 'MVis')
      CALL FILSYSEX (GSPFILE, TEMP)
      IF(FILEXIST(GSPFILE).AND.(TEMP.EQ.'SDE')) THEN
         CALL FILIMGGE ('Components', GSPFILE, ' ')
         CALL DATGETI ('Components', 'NITER', BITER, 1, NDUMMY)
         CALL DATPUTI ('Components', 'BITER', BITER+1, 1)
         CALL DATDELET ('Components/FLUX')
         CALL MSGPUT (
     1      'Restarting GSP from iteration '//STRINT(BITER), 'I')
      ELSE
         CALL IMGCLONE ('Dirty', 'Components')
         CALL ARRSETCO ('Components', 0.0, FLUX)
      END IF
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got images: '//CTIME, 'I')
C
C Call main routine
C
      CALL DATPUTI ('Components', 'NITER', NITER, 1)
      CALL DATPUTR ('Components', 'GAIN', GAIN, 1)
      CALL DATPUTR ('Components', 'ACCEL', ACCEL, 1)
      CALL IMGGSP ('Dirty', 'XFR', 'Components', 'Box', 'Residual',
     $   'MVis')
      CALL DATGETI ('Components', 'NITER', ANITER, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
      CALL ARRSTAT ('Residual', ' ')
      MAXRES = DATFGETR ('Residual','ARRMAX')
      MINRES = DATFGETR ('Residual','ARRMIN')
      IF (ABS(MINRES).GT.ABS(MAXRES)) MAXRES = MINRES
      WRITE (MESSAGE, 1000) ANITER, MAXRES
 1000 FORMAT ('Ran ',I6,' iterations: maximum residual ',
     1  1PE12.4,' Jy/beam')
      CALL MSGPUT (MESSAGE, 'I')
      CALL HISPUT ('Components', MESSAGE)
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Finished GSP: '//CTIME, 'I')
C
C Write answer: First do Components file
C
      IF (GSPFILE.NE.' ') THEN
         CALL DATPUTC ('Components', 'BUNIT', 'JY/PIXEL', 1)
         CALL DATPUTI ('Components', 'NITER', ANITER, 1)
         CALL FILIMGPU ('Components', GSPFILE, ' ')
      END IF
C
C Now do conventional GSP image if required
C
      IF (SGSPFILE.NE.' ') THEN
         CALL IMGCLONE ('Components', 'GSP')
         CALL IMGSMOOT ('Components', BEAM, 'GSP', 'MVis')
         CALL IMGP2PB ('GSP', BEAM, 'GSP')
         CALL ARRLC ('GSP', 1.0, 'Residual', 1.0, 'GSP')
         CALL DATPUTC ('GSP', 'BUNIT', 'JY/BEAM', 1)
         CALL FILIMGPU ('GSP', SGSPFILE, ' ')
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
