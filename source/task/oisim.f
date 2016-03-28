C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)oisim.f	1.7    5/13/91
C
      SUBROUTINE SDEMAIN
C
CD Program to make triple product data sets for simulated array data
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added photon noise capability
C				T.J.Cornwell	Oct 23 1989
C	Fixed VISPUT
C				T.J.Cornwell    Nov 1 1990
C	Now no longer tries to write table *
C				T.J.Cornwell    Dec 7 1990
C	Fixed TRPPUT
C				T.J.Cornwell    Dec 19 1990
C	Added TIME to SIMUV calls
C				M.A.Holdaway	May 12 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'OISIM')
C
      REAL                      CELLSIZE(3), SHIFT(3)
      REAL		          HALIMITS(2), DEC, ELMIN, INTTIME
      REAL	        	  PI
      REAL	       		  HMIN, HMAX, NPH
      PARAMETER                 (PI=3.14159274101257)
      CHARACTER*(SYSMXNAM)	TRPFILE, MODFILE, ANTFILE, MAPFILE,
     1				VISFILE, TELE, CTIME
      INTEGER		        NDUMMY, IMSIZE(3), NFRAMES
C
      REAL		FREQ, DATFGETR, TFLUX, AUTOW, TIME
C
      DATA           HALIMITS  /-.1, .1/
      DATA           DEC       /50./
      DATA           ELMIN     /30./
      DATA           INTTIME   /60./
      DATA           IMSIZE    /512, 512, 1/
      DATA           CELLSIZE  /1., 1., 1./
      DATA           SHIFT     /0., 0., 0./ 
C==================================================================
      CALL MSGWELCO ('I make simulated OI array data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 1, NDUMMY)
      CALL USRGETI('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETC('Map', MAPFILE, 1, NDUMMY)
      CALL USRGETR('HAlimits', HALIMITS, 2, NDUMMY)
      CALL USRGETR('Dec', DEC, 1, NDUMMY)
      CALL USRGETR('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR('ELmin', ELMIN, 1, NDUMMY)
      CALL USRGETR('INTtime', INTTIME, 1, NDUMMY)
      CALL USRGETR('Autow', AUTOW, 1, NDUMMY)
      CALL USRGETR('NPhotons', NPH, 1, NDUMMY)
      CALL USRGETI('NFrames', NFRAMES, 1, NDUMMY)
      CALL USRGETC('Telescope', TELE, 1, NDUMMY)
      CALL USRGETC('Triple', TRPFILE, 1, NDUMMY)
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETL('Debug',SYSDEBUG,1,NDUMMY) 
C
C Read the antennas file
C  
      CALL FILGETAN ('Antfile', ANTFILE)
C
      HMIN = MIN (HALIMITS(1), HALIMITS(2))
      HMAX = MAX (HALIMITS(1), HALIMITS(2))
      TIME = 0.0
C
C Now make empty data sets
C
      CALL MSGPUT ('Making initial datasets', 'I')
      CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX), 
     1   DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(INTTIME), 
     $   AUTOW.NE.0.0, AUTOW, 'Vis')
      CALL DATPUTC ('Vis', 'TELESCOP', TELE, 1)
      CALL MSGPUT ('Coordinates for visibility data', 'I')
      CALL CRDLIST ('Vis/OBS/I')
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
      IF(ERROR) GO TO 999
C
C Read the model file or image
C
      CALL MSGPUT ('Finding model image', 'I')
      IF (MODFILE.NE.' ') THEN
         CALL FILGETMO ('Model', MODFILE)
C
C Make an image
C
         CALL IMGMAKE ('Vis/OBS/I', CELLSIZE, IMSIZE, SHIFT, 
     1      'R', 'Map')
C
C Put the model in it
C
         CALL MODIMG ('Model', 'Map')
         IF (MAPFILE.NE.' ') THEN
            CALL FILIMGPU ('Map', MAPFILE, ' ')
         END IF
      ELSE
C
C Read the model image
C
         CALL FILIMGGE ('Map', MAPFILE, ' ')
      END IF
      IF(ERROR) GO TO 999
C
C Normalise the image so that the total flux is unity
C
      IF ((NPH.GT.0.0).AND.(NFRAMES.GT.0)) THEN
         CALL MSGPUT ('Normalizing image', 'I')
         CALL ARRSTAT ('Map', ' ')
         TFLUX = DATFGETR ('Map', 'ARRSUM')
         IF(TFLUX.NE.0.0) THEN
            CALL ARRSCALE ('Map', 1.0/TFLUX, 0.0, 'Map')
         ELSE
            CALL ERRREPOR (ROUTINE, ERRBDARG, 'Zero flux in model')
            GO TO 999
         END IF
      END IF
      IF(ERROR) GO TO 999
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got model: '//CTIME, 'I')
C
C List coordinates
C
      IF(ERROR) GO TO 999
      CALL MSGPUT ('Coordinates for image', 'I')
      CALL CRDLIST ('Map')
      IF (ERROR) GO TO 999
C
C Fill the visibility data set
C
      CALL MSGPUT ('Transforming model', 'I')
      CALL IMGTOVIS ('Vis', 'MOD/I', 'Map', 'Mvis', 'DMap')
      IF(ERROR) GO TO 999
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Transformed model: '//CTIME, 'I')
C
C Now write to disk
C
      IF (TRPFILE.NE.' ') THEN
C
C Now fill in the triple product data
C
         CALL MSGPUT ('Making and filling in triple product data set',
     $      'I')
         CALL SIMTRP (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX), 
     1      DBLE(DEC), DBLE(ELMIN), DBLE(INTTIME), AUTOW.NE.0.0,
     $      AUTOW, 'Triple')
         CALL DATPUTC ('Triple', 'TELESCOP', TELE, 1)
         CALL VISTOTRP ('Vis', 'MOD/I', 'OBS/I', 'Triple', 'OBS/I',
     $      NPH, NFRAMES)
         IF(ERROR) GO TO 999
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Filled in triple product data: '//CTIME, 'I')
         CALL TRPPUT ('Triple', TRPFILE, 'OBS', 'I', '*', ' ')
         IF (VISFILE.NE.' ') THEN
            CALL MSGPUT ('Writing amplitude estimates', 'I')
            CALL VISPUT ('Vis', VISFILE, 'OBS', 'I', '*', ' ')
         END IF
      END IF
C
 999  CONTINUE
      END
