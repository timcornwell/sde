C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to display visibility coverage
C
CS Arguments: CALL SDEMAIN
CA Audit trail:
CA	New task
CA					T.J. Cornwell	Feb 10, 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UVCOV')
C
      REAL		CELLSIZE(3), TAPER(3), MAXPSF,
     1			TIME(2), UVLIMITS(2), SHIFT(3), STOR, FOV
      INTEGER 		NDUMMY, I, DIR, NSEL, TIMR(8), IMSIZE(3)
      LOGICAL		GRIDCR, SLOWZ
      CHARACTER*(SYSMXNAM)	VISFILE, CTIME,
     1			STOKES, SUBCLASS, CFTYPE
      REAL	PIXR(2)
      DATA	PIXR/2*0.0/
C==================================================================
C
      CALL MSGWELCO ('I display uv coverage on the tv')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('Filter', TAPER, 3, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Subclass to be gridded
C
      SUBCLASS = 'OBS/'//STOKES(1:1)
C
C Now get relevant files
C
      CALL VISGET ('Vis', VISFILE, STOKES(1:1), '*', ' ')
C
      CALL UTLTTOD (TIMR(1), TIME(1))
      CALL UTLTTOD (TIMR(5), TIME(2))
      CALL VISSEL ('Vis', SUBCLASS, TIME, UVLIMITS, NSEL)
      IF (ERROR) GO TO 999
      WRITE (MESSAGE, 1000) NSEL
 1000 FORMAT ('Selected ',I7,' visibilities')
      CALL MSGPUT (MESSAGE, 'I')
C
C Make output image
C
      CALL IMGMAKE ('Vis/'//SUBCLASS, CELLSIZE, IMSIZE, SHIFT, 
     1   'R', 'PSF')
      IF (ERROR) GO TO 999
      CALL MSGPUT ('Coordinates for PSF:', 'I')
      CALL CRDLIST ('PSF')
C
C Find dual image
C
      IF (SLOWZ) THEN
         CALL MSGPUT ('Fourier sum in w', 'I')
         NDUMMY = 2
         CALL FFTCONJA ('PSF', 'XFR', DIR, NDUMMY)
         CALL DATPUTL ('PSF', 'SLOWZ', SLOWZ, 1)
         CALL DATPUTL ('XFR', 'SLOWZ', SLOWZ, 1)
      ELSE
         NDUMMY = 0
         CALL FFTCONJA ('PSF', 'XFR', DIR, NDUMMY)
      END IF
      CALL MSGPUT ('Coordinates for XFR:', 'I')
      CALL CRDLIST ('XFR')
      IF (ERROR) GO TO 999
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got data: '//CTIME, 'I')
C
C Now grid data and weights
C
      IF (FOV.NE.0.0) THEN
         CALL DATPUTR ('XFR', 'WTFOV', FOV, 1)
         CALL GRDUWT ('Vis', SUBCLASS, 'XFR')
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1100) FOV
 1100    FORMAT ('Weighting for fraction of field of view = ',
     1      F7.2)
         CALL MSGPUT (MESSAGE, 'I')
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Weighting completed: '//CTIME, 'I')
      END IF
C
C Apply taper
C
      IF ((TAPER(1).NE.0.0).OR.(TAPER(2).NE.0.0)) THEN
         CALL VISTAPER ('Vis', SUBCLASS, TAPER, SUBCLASS)
         IF (ERROR) GO TO 999
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Filtering: '//CTIME, 'I')
      END IF
C
C Do Box convolution onto grid
C
      CALL DATPUTC ('XFR', 'CFTYPE', 'BOX', 1)
      CALL VISGRID ('Vis', SUBCLASS, 'XFR', .TRUE.)
      CALL SYSETIME (CTIME)
      IF (ERROR) GO TO 999
      CALL MSGPUT ('Gridded data for XFR: '//CTIME, 'I')
C
C Now make into absolute value
C
      CALL ARRABS ('XFR', 'AXFR')
C
C Display on tv
C
      CALL IMGTVD ('AXFR', PIXR(1), PIXR(2))
C
 999  CONTINUE
      END
