C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)lucy.f	1.3    6/13/94
C
      SUBROUTINE SDEMAIN
C
CD Program to perform the Lucy algorithm of deconvolution
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added beam fitting
C				T.J. Cornwell	Sept 13 1989
C	Cleaned up by removing lots of extraneous stuff
C				T.J. Cornwell	July 25 1990
C	Changed units of component map to JY/PIXEL.  (Normal optical usage
C	seems to be to interpret this directly, as opposed to the radio
C	practice of post convolution and adding the residuals.)  This
C	really needs a decent model put in at the start, but failing that,
C	we'll use the very crude approximation of the box normalized to
C	TFLUX.  NB: Lucy iterations do *not* change the total flux!
C
C	I'm fairly sure that the flux in this task is still screwed up,
C	though it's no more screwed up than when I started messing with
C	it.  Someone really should sort this out properly someday.
C
C				D.S.Briggs	Nov 30 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'LUCY')
C
      INTEGER		NITER, BITER, ANITER, NDUMMY
      LOGICAL		FILEXIST
      CHARACTER*(SYSMXNAM)	LUCYFILE, DRTFILE, PSFFILE, BOXFILE,
     1			CTIME, STRINT
      REAL		S, DATFGETR, TFLUX
C==================================================================
      CALL MSGWELCO ('I perform Lucy deconvolution')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Box', BOXFILE, 1, NDUMMY)
      CALL USRGETR ('TFlux', TFLUX, 1, NDUMMY)
      CALL USRGETC ('Lucy', LUCYFILE, 1, NDUMMY)
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETL ('DEBUG', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Get images
C
      CALL FILIMGGE ('Dirty', DRTFILE, ' ')
      CALL ARRCLIP ('Dirty', 0.0, 1E20, 'Dirty')
      CALL MSGPUT ('Coordinates for dirty image', 'I')
      CALL CRDLIST ('Dirty')
C
      CALL IMGCLONE ('Dirty', 'Residual')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
      CALL ARRCLIP ('PSF', 0.0, 1E20, 'PSF')
C
      CALL ARRSTAT ('PSF', ' ')
      S = DATFGETR('PSF','ARRSUM')
      CALL ARRSCALE ('PSF', 1./S, 0.0, 'PSF')
C
      CALL ARRSTAT ('Dirty', ' ')
      S = DATFGETR('Dirty','ARRSUM')
      CALL ARRSCALE ('Dirty', TFLUX/S, 0.0, 'Dirty')
C
C Now make transfer functions, etc.
C
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL DATDELET ('PSF')
      CALL IMGCLONE ('XFR', 'MVis')
C
C Get old file if it exists
C
      IF(FILEXIST(LUCYFILE)) THEN
         CALL FILIMGGE ('Lucy', LUCYFILE, ' ')
         CALL DATGETI ('Lucy', 'NITER', BITER, 1, NDUMMY)
         CALL DATPUTI ('Lucy', 'BITER', BITER+1, 1)
         CALL MSGPUT (
     1      'Restarting LUCY from iteration '//STRINT(BITER), 'I')
      ELSE
         CALL FILMASGE ('Lucy', BOXFILE, 'Dirty')
         CALL ARRSTAT ('Lucy', ' ')
         S = DATFGETR('Lucy','ARRSUM')
         CALL ARRSCALE ('Lucy', TFLUX/S, 0.0, 'Lucy')
      END IF
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got images: '//CTIME, 'I')
C
C Call main routine
C
      CALL DATPUTI ('Lucy', 'NITER', NITER, 1)
      CALL IMGLUCY ('Dirty', 'XFR', 'Lucy', 'Residual', 'MVis')
      CALL DATGETI ('Lucy', 'NITER', ANITER, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Finished LUCY: '//CTIME, 'I')
C
C Write answer
C
      IF (LUCYFILE.NE.' ') THEN
         CALL DATPUTC ('Lucy', 'BUNIT', 'JY/PIXEL', 1)
         CALL DATPUTI ('Lucy', 'NITER', ANITER, 1)
         CALL FILIMGPU ('Lucy', LUCYFILE, ' ')
      END IF
C
 999  CONTINUE
      END
