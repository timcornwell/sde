C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissimr.f	1.1	 9/15/94
C
      SUBROUTINE SDEMAIN
C
CD Program to make simulated array data with RA,DEC (times are somewhat correct)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Sept 13 1994
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSIMR')
C
      REAL                      CELLSIZE(3), SHIFT(3)
      REAL		        LSTLIMIT(2), DEC, RA, ELMIN, INTTIME
      REAL			LONGITUD
      REAL			PHRMS, GRMS, GDRIFT, NRMS
      REAL	        	PI, TAU, TATM, TREC
      REAL	       		HMIN, HMAX, TFACT, TOFFSET
      REAL			GMST, GMST0H
      DOUBLE PRECISION		MJD, MJD0, REFDATE, SLAGMST, TU
      CHARACTER*(SYSMXNAM)	DATEOBS
      PARAMETER                 (PI=3.14159265358979)
      CHARACTER*(SYSMXNAM)	MODFILE, ANTFILE, MAPFILE, STOKES,
     $				VISFILE, CTIME, TELE
      INTEGER		        NDUMMY, IMSIZE(3), SEED(2)
C
      REAL		FREQ, AUTOW, TIME, PIXPC(2), WORLDPC(2),TELDIAM,
     $   		WT(3), D2R, EPOCH
      LOGICAL		ADDN, DOPB, DO3D, MAPCRDS, ZERO, DOVISMOD
C
      CHARACTER*8	CTYPE(SYSMXDIM)
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     $   		CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM), OBSRA, OBSDEC
C
      DATA           LSTLIMIT  /-.1, .1/
      DATA           DEC       /50./
      DATA           RA        /0./
      DATA           LONGITUD     /108./
      DATA           ELMIN     /30./
      DATA           INTTIME   /60./
      DATA           IMSIZE    /512, 512, 1/
      DATA           CELLSIZE  /1., 1., 1./
      DATA           SHIFT     /0., 0., 0./ 
      DATA	     TOFFSET   / 0.0  /
C
      DOUBLE PRECISION	DATFGETD
      LOGICAL		DATEXIST
C==================================================================
      CALL MSGWELCO ('I make simulated array data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETL ('DoVisModel', DOVISMOD, 1, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETC ('Map', MAPFILE, 1, NDUMMY)
      CALL USRGETL ('MapCoords', MAPCRDS, 1, NDUMMY)
      CALL USRGETR ('LSTlimits', LSTLIMIT, 2, NDUMMY)
      CALL USRGETR ('RA', RA, 1, NDUMMY)
      CALL USRGETR ('Dec', DEC, 1, NDUMMY)
      CALL USRGETR ('SiteLong', LONGITUD, 1, NDUMMY)
      CALL USRGETR ('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR ('ELmin', ELMIN, 1, NDUMMY)
      CALL USRGETR ('Autow', AUTOW, 1, NDUMMY)
      CALL USRGETR ('INTtime', INTTIME, 1, NDUMMY)
      CALL USRGETR ('Toffset', TOFFSET, 1, NDUMMY)
      CALL USRGETR ('Weight', WT, 3, NDUMMY)
      CALL USRGETR ('Tau', TAU, 1, NDUMMY)
      CALL USRGETR ('Tatm', TATM, 1, NDUMMY)
      CALL USRGETR ('Trec', TREC, 1, NDUMMY)
      CALL USRGETR ('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR ('PHRMS', PHRMS, 1, NDUMMY)
      CALL USRGETR ('NRMS', NRMS, 1, NDUMMY)
      CALL USRGETR ('GRMS', GRMS, 1, NDUMMY)
      CALL USRGETR ('GDRIFT', GDRIFT, 1, NDUMMY)
      CALL USRGETL ('DOPB', DOPB, 1, NDUMMY)
      CALL USRGETL ('DO3D', DO3D, 1, NDUMMY)
      CALL USRGETC ('Tele', TELE, 1, NDUMMY)
      CALL USRGETR ('Teldiam', TELDIAM, 1, NDUMMY)
      CALL USRGETR ('PixelPC', PIXPC, 2, NDUMMY)
      CALL USRGETL ('Zero', ZERO, 1, NDUMMY)
      CALL USRGETL ('ADDN', ADDN, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETL ('Debug',SYSDEBUG,1,NDUMMY) 
      CALL USRGETI ('Seed', SEED, 2, NDUMMY)
C
C These options are getting a little out of hand
C
      IF (DOVISMOD.AND.DOPB) THEN
         CALL MSGPUT ('I haven''t the foggiest what to do with this '
     $      // 'combination of options','E')
         GO TO 999
      END IF
C
C Read the antennas file
C  
      CALL FILGETAN ('Antfile', ANTFILE)
      CALL DATPUTR ('Antfile', 'Tau', TAU, 1)
      CALL DATPUTR ('Antfile', 'Tatm', TATM, 1)
      CALL DATPUTR ('Antfile', 'Trec', TREC, 1)
C
C  Calculate TIME, HMIN, HMAX
C
      HMIN = MIN (LSTLIMIT(1), LSTLIMIT(2)) - RA
      HMAX = MAX (LSTLIMIT(1), LSTLIMIT(2)) - RA
C
C  UT and GMST move at different rates
C
      TFACT = (360.0 + 360.0/365.25)/360.0
      HMIN = HMIN * TFACT + TOFFSET/ 3600.0
      HMAX = HMAX * TFACT + TOFFSET/ 3600.0
C
C Deal with time stuff!
C
      D2R = ATAN(1.0)/45.0
      DATEOBS = '01/01/50'
      EPOCH = 1950.0
C
      CALL UTLD3MJD (DATEOBS, MJD)
      CALL UTLD3MJD ('01/01/00', MJD0)
      REFDATE = MJD + 2400000.5
      TU = MJD - MJD0
C
      WRITE (MESSAGE, 111) REFDATE
 111  FORMAT ('JD Reference date is ',F10.2)
      CALL MSGPUT (MESSAGE, 'D')
      GMST0H = SLAGMST ( DBLE(MJD) ) /D2R /15.0
      WRITE (MESSAGE, 161) GMST0H
 161  FORMAT ('GMST0H as calculated = ',F15.8)
      CALL MSGPUT (MESSAGE, 'D') 
      GMST = HMIN + (RA + LONGITUD)/15.0
      TIME = MOD((24.0+GMST - GMST0H), 24) *
     $   (0.997269566414 -0.586E-10 * TU/36525.0)
      TIME = TIME/24.0
      IF (TIME .LT. 0.0) TIME = TIME + 1.0
      WRITE (MESSAGE, 121) TIME
 121  FORMAT ('Starting UT: ', F15.10)
      CALL MSGPUT (MESSAGE, 'D')
C
C Now make empty data sets
C
      CALL MSGPUT ('Making initial datasets', 'I')
      CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX),
     1     DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(INTTIME), 
     $     AUTOW.NE.0.0, AUTOW, 'Vis')
C
C Reweight it if requested
C
      IF ((WT(1).NE.1.0).OR.(WT(2).NE.0.0).OR.(WT(3).NE.0.0))
     $     CALL VISRWTDT ('Vis', 'OBS/I', 'Antfile', WT)
C
      CALL MSGPUT ('Coordinates for visibility data', 'I')
      CALL CRDGET ('Vis/OBS/I', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $     CROTA)
      CRVAL(1) = DBLE(RA)
      CALL CRDPUT ('Vis/OBS/I', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT, 
     $     CROTA)
      CALL CRDLISRD ('Vis/OBS/I')
      CALL CRDLIST ('Vis/OBS/I')
      IF(ERROR) GO TO 999
C
C Read the model file or image
C
      IF (ZERO) THEN
         CALL MSGPUT ('Model is ZERO','I')
         MAPCRDS = .FALSE.
      ELSE
         CALL MSGPUT ('Finding model image', 'I')
         IF ((MODFILE.NE.' ').OR.DOVISMOD) THEN
            CALL FILGETMO ('Model', MODFILE)
            CALL IMGMAKE ('Vis/OBS/I', CELLSIZE, IMSIZE, SHIFT, 
     1         'R', 'Map')
            CALL ARRSETCO('Map', 0.0, 0.0)
            IF (.NOT.DOVISMOD) THEN
               CALL MODIMG ('Model', 'Map')
               IF (MAPFILE.NE.' ') THEN
                  CALL FILIMGPU ('Map', MAPFILE, ' ')
               END IF
            END IF
         ELSE
            CALL FILIMGGE ('Map', MAPFILE, ' ')
         END IF
         IF(ERROR) GO TO 999
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Got model: '//CTIME, 'I')
         IF(ERROR) GO TO 999
      END IF
C
C Deal with coordinates
C
      OBSRA = DBLE(RA)
      OBSDEC = DBLE(DEC)
      IF (MAPCRDS) THEN
         IF (DATEXIST('Map/OBSRA'))
     $        OBSRA = DATFGETD ('Map', 'OBSRA')
         IF (DATEXIST('Map/OBSDEC'))
     $        OBSRA = DATFGETD ('Map', 'OBSDEC')
      END IF
      CALL DATPUTR ('Vis', 'REFDATE', EPOCH, 1)
      CALL DATPUTD ('Vis', 'OBSRA', OBSRA, 1)
      CALL DATPUTD ('Vis', 'OBSDEC', OBSDEC, 1)
C
      IF (.NOT.ZERO) THEN
         CALL DATPUTD ('Map', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Map', 'OBSDEC', OBSDEC, 1)
         CALL CRDGET ('Map', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $      CROTA)
         IF (.NOT.MAPCRDS) THEN
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
     $         CROTA)
         END IF
      END IF
C
C NB: the visibility data will have the same ref vals as the original
C database, if VisObs is used.  The coordinates in VIS/OBS/I take priority
C over the ones here.  If either VisObs or MapCoords is not used, they will
C be the same.
C
      CALL DATPUTC ('Vis', 'CTYPE', CTYPE, SYSMXDIM)
      CALL DATPUTD ('Vis', 'CRVAL', CRVAL, SYSMXDIM)
C
      IF (.NOT.ZERO) THEN
         CALL MSGPUT ('Coordinates for image', 'I')
         CALL CRDLISRD ('Map')
         CALL CRDLIST ('Map')
         IF (ERROR) GO TO 999
      END IF
C
C Fill the visibility data set
C
      IF (.NOT.ZERO) THEN
         IF (DOPB) THEN
            IF (PIXPC(1) .EQ. 0.0) PIXPC(1) = CRPIX(1)
            IF (PIXPC(2) .EQ. 0.0) PIXPC(2) = CRPIX(2)
            CALL CRDPTOW ('Map', PIXPC, WORLDPC)
            OBSRA = WORLDPC(1)
            OBSDEC = WORLDPC(2)
            CALL DATPUTD ('Map', 'OBSRA', OBSRA, 1)
            CALL DATPUTD ('Map', 'OBSDEC', OBSDEC, 1)
            CALL DATPUTC ('Map', 'TELESCOP', TELE, 1)
            CALL DATPUTD ('Vis', 'OBSRA', OBSRA, 1)
            CALL DATPUTD ('Vis', 'OBSDEC', OBSDEC, 1)
            CALL DATPUTC ('Vis', 'TELESCOP', TELE, 1)
            IF(TELE(1:4).EQ.'AIRY') THEN
               CALL DATPUTR ('Map', 'TELDIAM', TELDIAM, 1)
               CALL DATPUTR ('Vis', 'TELDIAM', TELDIAM, 1)
            END IF
C
            CALL IMGCLONE ('Map', 'MapPB')
            CALL IMGPB ('Map', 'MapPB', 'APPLY')
            CALL ARRCOPY ('MapPB', 'Map')
            CALL DATDELET ('MapPB')
         ENDIF
         IF(DO3D) THEN
            CALL MSGPUT ('Transforming model', 'I')
            CALL IMG3FT ('Vis', 'OBS/I', 'Map')
         ELSE
            IF (DOVISMOD) THEN
               CALL MSGPUT ('Calculating analytic model transform','I')
               CALL MODVIS ('Model', 'Map', 'Vis', 'OBS/I')
            ELSE
               CALL MSGPUT ('Transforming model', 'I')
               CALL IMGTOVIS ('Vis', 'OBS/I', 'Map', 'Mvis', 'DMap')
            END IF
         END IF
         IF(ERROR) GO TO 999
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Transformed model: '//CTIME, 'I')
      END IF
C
C Change STOKES
C
      CALL DATPUTC ('Vis', 'ANTNOISE', 'YES', 1)
      IF(STOKES.EQ.'I') THEN
         IF(ADDN) THEN
            CALL MSGPUT ('Corrupting Stokes I', 'I')
            CALL VISCORRU ('Vis', PHRMS, GRMS, GDRIFT, NRMS, 
     $         SEED, 'OBS/I', 'OBS/I')
         END IF
      ELSE IF(STOKES.EQ.'IV') THEN
         CALL DATCREAT ('Vis/OBS/V')
         CALL ARRCOPY ('Vis/OBS/I/VIS', 'Vis/OBS/V/VIS')
         CALL ARRCOPY ('Vis/OBS/I/WT',  'Vis/OBS/V/WT')
         CALL CRDGET ('Vis/OBS/I', NAX, CTYPE, NAXIS, CRVAL, CRPIX, 
     $      CDELT, CROTA)
         CALL CRDPUT ('Vis/OBS/V', NAX, CTYPE, NAXIS, CRVAL, CRPIX, 
     $      CDELT, CROTA)
         IF(ADDN) THEN
            CALL DATCREAT ('Vis/OBS/RR')
            CALL DATCREAT ('Vis/OBS/LL')
            CALL ARRCOPY ('Vis/OBS/I/VIS', 'Vis/OBS/RR/VIS')
            CALL ARRCOPY ('Vis/OBS/I/WT',  'Vis/OBS/RR/WT')
            CALL ARRCOPY ('Vis/OBS/I/VIS', 'Vis/OBS/LL/VIS')
            CALL ARRCOPY ('Vis/OBS/I/WT',  'Vis/OBS/LL/WT')
            CALL CRDPUT ('Vis/OBS/RR', NAX, CTYPE, NAXIS, CRVAL, CRPIX, 
     $         CDELT, CROTA)
            CALL CRDPUT ('Vis/OBS/LL', NAX, CTYPE, NAXIS, CRVAL, CRPIX, 
     $         CDELT, CROTA)
            CALL MSGPUT ('Corrupting Stokes RR', 'I')
            CALL VISCORRU ('Vis', PHRMS, GRMS, GDRIFT, NRMS, 
     $         SEED(1), 'OBS/I', 'OBS/RR')
            CALL MSGPUT ('Corrupting Stokes LL', 'I')
            CALL VISCORRU ('Vis', PHRMS, GRMS, GDRIFT, NRMS, 
     $         SEED(2), 'OBS/I', 'OBS/LL')
            CALL ARRLC ('Vis/OBS/RR/VIS', 0.5, 'Vis/OBS/LL/VIS', 0.5,
     &         'Vis/OBS/I/VIS')
            CALL ARRLC ('Vis/OBS/RR/VIS', 0.5, 'Vis/OBS/LL/VIS', -0.5,
     &         'Vis/OBS/V/VIS')
         ELSE
            CALL ARRSETCO ('Vis/OBS/V/VIS', 0.0, 0.0)
         END IF
      END IF
C
C Now write to disk
C
      IF (VISFILE.NE.' ') THEN
         CALL HISINPUT('Vis')
         CALL VISPUT ('Vis', VISFILE, 'OBS', STOKES, '*', ' ')
      END IF
C
 999  CONTINUE
      END
