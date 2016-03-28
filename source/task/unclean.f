C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)unclean.f	1.3    24 Feb 1995
C
      SUBROUTINE SDEMAIN
C
C Program to 'back up' an existing CLEAN.  A single image may be backed up
C to remove the effects of overcleaning, or a series of CLEAN images can
C be generated to study the effect of algorithm convergence.  In addition,
C it can be used to regenerate the CLEAN image, RESIDUAL image, or ASCII
C clean component list from the basic SDE format clean workfile.
C
C Audit trail:
C       Cloned from clean.f
C                               D.S.Briggs      January 31, 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UNCLEAN')
C
      INTEGER           NIBEGIN, NIEND, BLC(SYSMXDIM), TRC(SYSMXDIM)
      REAL              NIINC
      CHARACTER*(SYSMXNAM)	ICMPFILE, OCMPFILE, RESFILE, DRTFILE,
     $                  PSFFILE, CLNFILE, CCFILE, CLNTYPE
C
      INTEGER           MAXITER
      INTEGER		NITER, ANITER, NDUMMY, BP,
     $			STEP (SYSMXDIM), DIR
      REAL		BEAM(4), RNITER
      CHARACTER*(SYSMXNAM)      TMPFILE, CTIME
C
      DATA		STEP	/SYSMXDIM*1/
C
      INTEGER           STRLEN
      REAL              DATFGETR
      LOGICAL           DATEXIST
C==================================================================
      CALL MSGWELCO ('I partially reverse a CLEAN deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('InComponents', ICMPFILE, 1, NDUMMY)
      CALL USRGETC ('OutComponents', OCMPFILE, 1, NDUMMY)
      CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
      CALL USRGETC ('CLEAN', CLNFILE, 1, NDUMMY)
      CALL USRGETC ('CCfile', CCFILE, 1, NDUMMY)
      CALL USRGETC ('Algorithm', CLNTYPE, 1, NDUMMY)
      IF (ERROR) GO TO 999
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('NIbegin', NIBEGIN, 1, NDUMMY)
      CALL USRGETI ('NIend', NIEND, 1, NDUMMY)
      CALL USRGETR ('NIinc', NIINC, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Hogbom or Clark?
C
      IF (CLNTYPE.EQ.'CLARK') THEN
         CALL MSGPUT ('Clark algorithm', 'I')
      ELSE
         CALL MSGPUT ('Hogbom algorithm', 'I')
      END IF
C
C Get images
C
      CALL FILIMGGE ('InitialDirty', DRTFILE, ' ')
C
C Cut out inner part of Dirty image
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
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
C
C Fit a Gaussian Beam to PSF
C
      IF (BEAM(1) .EQ. 0.0) THEN
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
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL FFTCONJA ('PSF', 'MVis', DIR, 0)
      IF(CLNTYPE.EQ.'CLARK') THEN
         CALL IMGSUBPS ('PSF', 'SPSF', BP)
         CALL DATDELET ('PSF')
      END IF
C
      CALL FILIMGGE ('Components', ICMPFILE, ' ')
      CALL DATGETI ('Components', 'NITER', MAXITER, 1, NDUMMY)
      WRITE (MESSAGE, 1010) MAXITER
 1010 FORMAT ('Found',I7,' components')
      CALL MSGPUT (MESSAGE, 'I')
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got images: '//CTIME, 'I')
C
C Add history cards
C
      CALL HISOPEN ('Components')
      CALL HISINPUT ('Components')
C
C Setup for loop.  (A little convoluted, I grant you.)
C
      IF (NIBEGIN.LE.0) THEN
         IF (NIEND.LE.0) THEN
            NIBEGIN = MAXITER
         ELSE
            NIBEGIN = 1
         END IF
      END IF
      NIBEGIN = MIN (NIBEGIN, MAXITER)
      NIEND = MIN (NIEND, MAXITER)
      NIEND = MAX (NIEND, NIBEGIN)
      RNITER = NIBEGIN
      NITER = NINT(RNITER)
C
C Main Loop begins here
C
 100  CONTINUE
C
C This is rather a kludge.  Instead of making a new components image,
C which would be the elegant way to do it, we simply poke a new value of
C NITER into the existing one.  This means that the output files may well
C be considerably larger than they need to be, if they are saved as .SDE
C In a similar vein, the history information will not be correct.
C
      CALL DATPUTI ('Components', 'NITER', NITER, 1)
      WRITE (MESSAGE, 1100) NITER
 1100 FORMAT ('Iteration number',I7)
      CALL MSGPUT (MESSAGE, 'I')
C
C Recalculate images as needed
C
      IF ((OCMPFILE.NE.' ').OR.(CLNFILE.NE.' ').OR.
     $   (RESFILE.NE.' ')) THEN
         CALL IMGCCIMG ('Components')
         CALL DATPUTI ('Components', 'NITER', ANITER, 1)
         CALL DATPUTC ('Components', 'BUNIT', 'JY/PIXEL', 1)
      END IF
C
      IF ((CLNFILE.NE.' ').OR.(RESFILE.NE.' ')) THEN
         CALL IMGRESID ('Components', 'Dirty', 'XFR', 'Residual',
     $      'MVis')
         CALL DATPUTC ('Residual', 'BUNIT', 'JY/BEAM', 1)
      END IF
C
      IF (CLNFILE.NE.' ') THEN
         IF (DATEXIST('Clean')) CALL DATDELET('Clean')
         CALL IMGCLONE ('Components', 'Clean')
         CALL IMGSMOOT ('Components', BEAM, 'Clean', 'MVis')
         CALL IMGP2PB ('Clean', BEAM, 'Clean')
         CALL ARRLC ('Clean', 1.0, 'Residual', 1.0, 'Clean')
         CALL DATPUTC ('Clean', 'BUNIT', 'JY/BEAM', 1)
         CALL DATPUTI ('Components', 'NITER', ANITER, 1)
      END IF
C
C Write requested images to disk
C
C First, the new components file
C
      IF (OCMPFILE.NE.' ') THEN
         CALL STRNUMFL (OCMPFILE, NITER, TMPFILE)
         CALL FILIMGPU ('Components', TMPFILE, ' ')
      END IF
C
C Write CC file if required
C
      IF (CCFILE.NE.' ') THEN
         IF (OCMPFILE.NE.' ') THEN
            CALL STRNUMFL (OCMPFILE, NITER, TMPFILE)
            WRITE (STRBUF, 2010) TMPFILE(1:STRLEN(TMPFILE))
         ELSE
            WRITE (STRBUF, 2010) CLNFILE(1:STRLEN(CLNFILE))
         END IF
 2010    FORMAT ('/ CC list for image ',A)
         CALL STRNUMFL (CCFILE, NITER, TMPFILE)
         WRITE (MESSAGE, 2020) TMPFILE(1:STRLEN(TMPFILE))
 2020    FORMAT ('Writing ASCII clean components to file ',A)
         CALL MSGPUT (MESSAGE,'I')
         CALL IMGCLIS ('Components', TMPFILE, STRBUF)
      END IF
C
C Now do conventional CLEAN image
C
      IF (CLNFILE.NE.' ') THEN
         CALL STRNUMFL (CLNFILE, NITER, TMPFILE)
         CALL FILIMGPU ('Clean', TMPFILE, ' ')
      END IF
C
C Residual image
C
      IF (RESFILE.NE.' ') THEN
         CALL STRNUMFL (RESFILE, NITER, TMPFILE)
         CALL FILIMGPU ('Residual', TMPFILE, ' ')
      END IF
C
C Calculate next value of NITER, and possibly loopback
C
      IF (NIINC .LT. 0.0) THEN
 150     RNITER = RNITER * ABS(NIINC)
         IF (NINT(RNITER).EQ.NITER) GO TO 150
         NITER = NINT(RNITER)
      ELSE IF (NIINC .GT. 0.0) THEN
         RNITER = RNITER + NIINC
         NITER = NINT(RNITER)
      ELSE
         NITER = NIEND + 1
      END IF
C
      IF (NITER .LE. NIEND) GO TO 100
C
 999  CONTINUE
      END
