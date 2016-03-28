C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissub.f	1.1    7/15/93
C
      SUBROUTINE SDEMAIN
C
CD Program to make simulated array data
C
C Audit trail:
C	Original version: Roughly cloned from vissim
C				D.S.Briggs	13 July 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSUB')
C
      REAL                      CELLSIZE(3), SHIFT(3), FACTOR(2)
      CHARACTER*(SYSMXNAM)	MODFILE, MAPFILE,
     1				VISFILE, CTIME, OUTFILE
      INTEGER		        NDUMMY, IMSIZE(3)
C
      LOGICAL		DO3D, DODFT, MAPCRDS
C
      CHARACTER*8	CTYPE(SYSMXDIM)
      INTEGER		NAX, NAXIS(SYSMXDIM), I
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     $   		CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM), OBSRA, OBSDEC, FREQ
C
      INTEGER		STRSEARC
C==================================================================
      CALL MSGWELCO ('I make simulated array data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETC ('Map', MAPFILE, 1, NDUMMY)
      CALL USRGETR ('Factor', FACTOR, 2, NDUMMY)
      CALL USRGETL ('DFT', DODFT, 1, NDUMMY)
      CALL USRGETL ('MapCoords', MAPCRDS, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY) 
C
C Get the visibility data
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
      CALL MSGPUT ('Coordinates for visibility data', 'I')
      CALL CRDLISRD ('Vis/OBS/I')
      CALL CRDLIST ('Vis/OBS/I')
      IF(ERROR) GO TO 999
C
C Read the model file or image
C
      CALL MSGPUT ('Finding model image', 'I')
      IF (MODFILE.NE.' ') THEN
         CALL FILGETMO ('Model', MODFILE)
         CALL IMGMAKE ('Vis/OBS/I', CELLSIZE, IMSIZE, SHIFT, 
     1      'R', 'Map')
         CALL ARRSETCO('Map', 0.0, 0.0)
         CALL MODIMG ('Model', 'Map')
         IF (MAPFILE.NE.' ') THEN
            CALL FILIMGPU ('Map', MAPFILE, ' ')
         END IF
      ELSE
         CALL FILIMGGE ('Map', MAPFILE, ' ')
      END IF
      IF(ERROR) GO TO 999
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got model: '//CTIME, 'I')
      IF(ERROR) GO TO 999
C
C Deal with coordinates
C
      IF (.NOT.MAPCRDS) THEN
         CALL CRDGET ('Vis/OBS/I', NAX, CTYPE, NAXIS, CRVAL, CRPIX,
     $      CDELT, CROTA)
         I = STRSEARC('RA', CTYPE, NAX)
         IF (I.EQ.0) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Can''t find Vis RA')
            GO TO 999
         END IF
         OBSRA = CRVAL(I)
         I = STRSEARC('DEC', CTYPE, NAX)
         IF (I.EQ.0) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Can''t find Vis DEC')
            GO TO 999
         END IF
         OBSDEC = CRVAL(I)
         I = STRSEARC('FREQ', CTYPE, NAX)
         IF (I.EQ.0) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Can''t find Vis FREQ')
            GO TO 999
         END IF
         FREQ = CRVAL(I)
C         
         CALL CRDGET ('Map', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $      CROTA)
         I = STRSEARC('RA---SIN', CTYPE, NAX)
         IF (I.EQ.0) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Can''t find Img RA')
            GO TO 999
         END IF
         CRVAL(I) = OBSRA
         I = STRSEARC('DEC--SIN', CTYPE, NAX)
         IF (I.EQ.0) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Can''t find Img DEC')
            GO TO 999
         END IF
         CRVAL(I) = OBSDEC
         I = STRSEARC('FREQ', CTYPE, NAX)
         IF (I.EQ.0) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Can''t find Img FREQ')
            GO TO 999
         END IF
         CRVAL(I) = FREQ
         CDELT(I) = .1 * FREQ
         CROTA(3) = 0.
C
         CALL CRDPUT ('Map', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $   CROTA)
      END IF
C
      CALL MSGPUT ('Coordinates for image', 'I')
      CALL CRDLISRD ('Map')
      CALL CRDLIST ('Map')
      IF (ERROR) GO TO 999
C
C Save the original visibilities
C
      CALL ARRCOPY ('Vis/OBS/I/VIS', 'TempVis')
C
C Fill the visibility data set
C
      CALL MSGPUT ('Transforming model', 'I')
      IF (DODFT) THEN
         CALL IMGSTRIM ('Map')
         CALL MSGPUT ('Coordinates for trimmed image', 'I')
         CALL CRDLISRD ('Map')
         CALL CRDLIST ('Map')
         CALL IMGDFT ('Map', 'Vis', 'OBS/I')
      ELSE
         IF(DO3D) THEN
            CALL IMG3FT ('Vis', 'OBS/I', 'Map')
         ELSE
            CALL IMGTOVIS ('Vis', 'OBS/I', 'Map', 'Mvis', 'DMap')
         END IF
      END IF
      IF(ERROR) GO TO 999
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Transformed model: '//CTIME, 'I')
C
      CALL ARRLC ('Vis/OBS/I/VIS', FACTOR(2), 'TempVis', FACTOR(1),
     $   'Vis/OBS/I/VIS')
C
C Now write to disk
C
      CALL HISINPUT ('Vis')
      IF (VISFILE.NE.' ') THEN
         CALL HISINPUT('Vis')
         CALL VISPUT ('Vis', OUTFILE, 'OBS', 'I', '*', ' ')
      END IF
C
 999  CONTINUE
      END
