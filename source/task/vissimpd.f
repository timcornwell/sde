C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissimpd.f	1.3	 24 Feb 1995
C
      SUBROUTINE SDEMAIN
C
CD Program to make simulated array data
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C	Clones from VISSIM
C					M.A.Holdaway	Sept 5 1992
C	Added PFRAC, enabling Q to have some fraction of I in it
C					M.A.Holdaway	Sept 18 1992
C	Adjusted HALIM so that is comes out right after we adjust
C	the HA for LST/UT in SIMUV
C					M.A.Holdaway	Sept 24 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSIMPD')
C
      REAL                      CELLSIZE(3), SHIFT(3)
      REAL		        HALIMITS(2), DEC, ELMIN, INTTIME
      REAL			NRMS, ACDRIFT
      REAL	        	PI
      REAL	       		HMIN, HMAX
      PARAMETER                 (PI=3.14159265358979)
      CHARACTER*(SYSMXNAM)	MODFILE, ANTFILE, MAPFILE,
     1				VISFILE, CTIME, DTFILE, ADTFILE
      INTEGER		        NDUMMY, IMSIZE(3), SEED
C
      REAL		FREQ, AUTOW, TIME, D2R, TFACT
C
      CHARACTER*8	CTYPE(SYSMXDIM)
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     $   		CROTA(SYSMXDIM)
      REAL		EPOCH, RA, SLON, SLAT, GMST, GMST0H
      DOUBLE PRECISION	CRVAL(SYSMXDIM), OBSRA, OBSDEC, MJD, MJD0,
     $   		REFDATE, SLAGMST, TU
      CHARACTER*(SYSMXNAM)	DTERMS, DATEOBS
      REAL			DRMS, DRIFTRMS, DTINT, PFRAC, CFRAC
C
      DATA           HALIMITS  /-.1, .1/
      DATA           DEC       /50./
      DATA           ELMIN     /30./
      DATA           INTTIME   /60./
      DATA           IMSIZE    /512, 512, 1/
      DATA           CELLSIZE  /1., 1., 1./
      DATA           SHIFT     /0., 0., 0./ 
C
C==================================================================
      CALL MSGWELCO ('I make simulated array data with D terms')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETC ('Map', MAPFILE, 1, NDUMMY)
      CALL USRGETR ('Pfrac', PFRAC, 1, NDUMMY)
      CALL USRGETR ('HAlimits', HALIMITS, 2, NDUMMY)
      CALL USRGETR ('Dec', DEC, 1, NDUMMY)
      CALL USRGETR ('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR ('ELmin', ELMIN, 1, NDUMMY)
      CALL USRGETR ('Autow', AUTOW, 1, NDUMMY)
      CALL USRGETR ('INTtime', INTTIME, 1, NDUMMY)
      CALL USRGETR ('DRMS', DRMS, 1, NDUMMY)
      CALL USRGETR ('DriftRMS', DRIFTRMS, 1, NDUMMY)
      CALL USRGETR ('NRMS', NRMS, 1, NDUMMY)
      CALL USRGETR ('Cfrac', CFRAC, 1, NDUMMY)
      CALL USRGETR ('ACdrift', ACDRIFT, 1, NDUMMY)
      CALL USRGETR ('DTINT', DTINT, 1, NDUMMY)
      CALL USRGETI ('SEED', SEED, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('DTerms', DTFILE, 1, NDUMMY)
      CALL USRGETC ('ADTerms',  ADTFILE, 1, NDUMMY)
      CALL USRGETL ('Debug',SYSDEBUG,1,NDUMMY) 
C
C Read the antennas file
C  
      CALL FILGETAN ('Antfile', ANTFILE)
C
      HMIN = MIN (HALIMITS(1), HALIMITS(2))
      HMAX = MAX (HALIMITS(1), HALIMITS(2))
      TFACT = (360.0 + 360.0/365.25)/360.0
      HMIN = HMIN * TFACT
      HMAX = HMAX * TFACT
      TIME = 0.0
C
C Deal with time stuff!
C
      D2R = ATAN(1.0)/45.0
      DATEOBS = '01/01/50'
      EPOCH = 1950.0
      RA = 0.0
      CALL UTLD3MJD (DATEOBS, MJD)
      CALL UTLD3MJD ('01/01/00', MJD0)
      REFDATE = MJD + 2400000.5
      TU = MJD - MJD0
C
C VLA Longitude and Lattitude
C
      SLON = 107.6177275
      SLAT = 34.078749167
C
      WRITE (MESSAGE, 111) REFDATE
 111  FORMAT ('JD Reference date is ',F10.2)
      CALL MSGPUT (MESSAGE, 'D')
      GMST0H = SLAGMST ( DBLE(MJD) ) /D2R /15.0
      WRITE (MESSAGE, 161) GMST0H
 161  FORMAT ('GMST0H as calculated = ',F15.8)
      CALL MSGPUT (MESSAGE, 'D') 
      GMST = HMIN + (RA + SLON)/15.0
      TIME = MOD((24.0+GMST - GMST0H), 24.0) *
     $   (0.997269566414 -0.586E-10 * TU/36525.0)
      TIME = TIME/24.0
      WRITE (MESSAGE, 121) TIME
 121  FORMAT ('Starting UT: ', F15.10)
      CALL MSGPUT (MESSAGE, 'D')
C
C Now make empty data sets
C
      CALL MSGPUT ('Making initial datasets', 'I')
      CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX),
     1   DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(INTTIME), 
     $   AUTOW.NE.0.0, AUTOW, 'Vis')
C
      CALL MSGPUT ('Coordinates for visibility data', 'I')
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
      OBSRA = 0.D0
      OBSDEC = DBLE(DEC)
      CALL DATPUTR ('Vis', 'EPOCH', 1950.0, 1)
      CALL DATPUTR ('Vis', 'REFDATE', 1950.0, 1)
      CALL DATPUTC ('Vis', 'DATE-OBS', DATEOBS, 1)
      CALL DATPUTD ('Vis', 'OBSRA', OBSRA, 1)
      CALL DATPUTD ('Vis', 'OBSDEC', OBSDEC, 1)
      CALL DATPUTD ('Map', 'OBSRA', OBSRA, 1)
      CALL DATPUTD ('Map', 'OBSDEC', OBSDEC, 1)
      CALL CRDGET ('Map', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $   CROTA)
      NAX = 3
      CRVAL(1) = OBSRA
      CRVAL(2) = OBSDEC
      CRVAL(3) = DBLE(FREQ)
      CTYPE(3) = 'FREQ'
      NAXIS(3) = 1
      CRPIX(3) = 1.
      CDELT(3) = 0.1 * FREQ
      CROTA(3) = 0.
      CALL CRDPUT ('Map', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $   CROTA)
      CALL DATPUTC ('Vis', 'CTYPE', CTYPE, SYSMXDIM)
      CALL DATPUTD ('Vis', 'CRVAL', CRVAL, SYSMXDIM)
C
      CALL MSGPUT ('Coordinates for image', 'I')
      CALL CRDLIST ('Map')
      IF (ERROR) GO TO 999
C
C Fill the visibility data set
C
      CALL MSGPUT ('Transforming model', 'I')
      CALL IMGTOVIS ('Vis', 'OBS/I', 'Map', 'Mvis', 'DMap')
      IF(ERROR) GO TO 999
      CALL SYSETIME (CTIME)
      CALL VISSCLON ('Vis/OBS', 'I', 'Q')
      CALL VISSCLON ('Vis/OBS', 'I', 'U')
      CALL VISSCLON ('Vis/OBS', 'I', 'V')
      IF (ERROR) GOTO 999
      CALL ARRSCALE ('Vis/OBS/I/VIS', PFRAC, 0.0, 'Vis/OBS/Q/VIS')
      CALL ARRLIST ('Vis/OBS/I/VIS', 'I', 0)
      CALL ARRLIST ('Vis/OBS/Q/VIS', 'Q', 0)
      CALL ARRSETCO ('Vis/OBS/U/VIS', 0.0, 0.0)
      CALL ARRSETCO ('Vis/OBS/V/VIS', 0.0, 0.0)
      IF (ERROR) GOTO 999
      CALL MSGPUT ('Transformed model: '//CTIME, 'I')
C
C Now corrupt
C
      DTERMS = 'Vis/DTERMS'
      CALL DATCREAT (DTERMS)
      CALL DATPUTR (DTERMS, 'DRMS', DRMS, 1)
      CALL DATPUTR (DTERMS, 'DRIFTRMS', DRIFTRMS, 1)
      CALL DATPUTR (DTERMS, 'ACDRIFT', ACDRIFT, 1)
      CALL DATPUTR (DTERMS, 'TINT', DTINT, 1)
      CALL DATPUTR (DTERMS, 'NRMS', NRMS, 1)
      CALL DATPUTR (DTERMS, 'CorrFrac', CFRAC, 1)
      CALL DATPUTI (DTERMS, 'SEED', SEED, 1)
      CALL DATPUTR ('Vis', 'SLON', SLON, 1)
      CALL DATPUTR ('Vis', 'SLAT', SLAT, 1)
      CALL VSDCORR ('Vis', DTERMS, 'OBS', 'OBS')
C
C Now write to disk
C
      IF (VISFILE.NE.' ') THEN
         CALL DATSETTP ('Vis', 'VIS')
         CALL VISPUT ('Vis', VISFILE, 'OBS', '*', '*', ' ')
      END IF
      IF (DTFILE .NE. ' ') THEN
         CALL VSDPUT (DTERMS, DTFILE)
      ENDIF
      IF (ADTFILE .NE. ' ') THEN
         CALL VSDPUT (DTERMS, ADTFILE)
      ENDIF
C
 999  CONTINUE
      END
