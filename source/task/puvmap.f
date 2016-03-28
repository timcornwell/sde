C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)puvmap.f	1.1 12/5/92
C
      SUBROUTINE SDEMAIN
C
CD Program to map visibility data in parallel
C
C Audit trail:
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PUVMAP')
C
      REAL		CELLSIZE(3), TAPER(3), MAXPSF,
     1			TIME(2), UVLIMITS(2), SHIFT(3), FOV
      INTEGER 		NDUMMY, DIR, NSEL, TIMR(8), IMSIZE(3)
      INTEGER		I
      LOGICAL		SLOWZ, DATEXIST, DFT, PSF(2)
      CHARACTER*(SYSMXNAM)	VISFILE, DRTFILE, PSFFILE, CTIME,
     1			STOKES, SUBCLASS, CFTYPE, VIS(2), GRID(2),
     $   		IMG(2), SUB(2)
      DATA		IMG /'PSF', 'Dirty'/
C==================================================================
C
      CALL MSGWELCO ('I make images from visibility data in parallel')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 3, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETC ('ConvType', CFTYPE, 1, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('Filter', TAPER, 3, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETL ('SlowZ', SLOWZ, 1, NDUMMY)
      CALL USRGETL ('DFT', DFT, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
      SLOWZ = SLOWZ .AND. (IMSIZE(3).GT.1)
C
C Subclass to be gridded
C
      SUBCLASS = 'OBS/'//STOKES(1:1)
C
C Now get relevant files
C
      CALL VISGET ('Vis', VISFILE, STOKES(1:1), '*', ' ')
C
      IF(DATEXIST('Vis/TIME')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL ('Vis', SUBCLASS, TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No TIME information', 'W')
      END IF
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
      IF (ERROR) GO TO 999
C
C DFT or FFT?
C
      IF (DFT) THEN
         CALL IMGCLONE ('PSF', 'Dirty')
         CALL VISDFT   ('Vis', SUBCLASS, 'Dirty', 'PSF')
         IF (ERROR) GO TO 999
      ELSE
C
         CALL DATPUTC ('PSF', 'CFTYPE', CFTYPE, 1)
         CALL DATPUTC ('XFR', 'CFTYPE', CFTYPE, 1)
         CALL IMGCLONE ('PSF', 'Dirty')
         IF (ERROR) GO TO 999
C
C Now load up arrays for parallel VISTOIMG
C
         DO 9998 I = 1, 2
            VIS(I)='Vis'
            SUB(I)=SUBCLASS
            GRID(I)='XFR'
            PSF(I)=I.EQ.1
 9998    CONTINUE
C
         CALL VISTOIMGH (2, VIS, SUB, IMG, GRID, PSF)
C
         CALL ARRSTAT ('PSF', ' ')
         CALL DATGETR ('PSF', 'ARRMAX', MAXPSF, 1, NDUMMY)
         IF (MAXPSF.EQ.0.0) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'Peak of PSF zero')
            GO TO 999
         ELSE
            CALL ARRSCALE('PSF',1.0/MAXPSF,0.0,'PSF')
            CALL ARRSCALE('Dirty',1.0/MAXPSF,0.0,'Dirty')
         END IF
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Made PSF: '//CTIME, 'I')
         IF (ERROR) GO TO 999
      ENDIF
C
C Write answer
C
      IF (PSFFILE.NE.' ') THEN         
         CALL HISOPEN ('PSF')
         CALL HISINPUT ('PSF')
         CALL FILIMGPU ('PSF', PSFFILE, ' ')
      ENDIF
      IF (DRTFILE.NE.' ') THEN
         CALL FILIMGPU ('Dirty', DRTFILE, ' ')
      ENDIF
C
 999  CONTINUE
      END
