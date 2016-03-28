C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissim.f	1.23	 11/21/94
C
      SUBROUTINE SDEMAIN
C
CD Program to make simulated array data
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell    Nov 2 1990
C	Slightly cleaned up and made to work (except for noise)
C				R.G. Marson     Nov 18 1990
C	Actually read the integration time
C				T.J.Cornwell    Feb 1 1991
C	Some more header items were needed in the image
C				M.A.Holdaway	March 19 1991
C	Added PB.  But thats only one PB.  We haven't become MOSSIM yet.
C				M.A.Holdaway	April 30 1991
C	Pass SEED down to VISCORRU
C				M.A.Holdaway	May 1 1991
C	Hey! I'm getting to be a regular around here.
C	Pass Starting TIME down to SIMUV
C				M.A.Holdaway	May 12 1991
C	Added VisObs input, to facilitate simulating an existing
C	observational dataset.  The weights are left as in the original
C	dataset.
C				D.S.Briggs	Jul 5 1992
C	Added STOKES input to allow reasonable simulation of polarization
C	effects
C				T.J.Cornwell    Sept 9 1992
C	Added MapCoord logical input, to control whether the model
C	image coordinates are used to position the source model, or
C	if the model image is simply assumed to be centered at
C	OBSRA & OBSDEC.  Bugfix in VisObs feature.  Use existing
C	weights if NRMS < 0.
C				D.S.Briggs	May 4 1993
C	Added ZERO option, to suppress calculation of visibilities
C	if not needed.  (Noise may still be added)
C				D.S.Briggs	Aug 28 1993
C	Added Weight option to control initial weights as a function
C	of dish diameter and integration time.  Tweaked VisObs feature
C	to allow simulation of existing Caltech data sets.
C				D.S.Briggs	Oct 31 1993
C	Added ability to do model calculations in visibility plane, which
C	removes gridding as a source of model error.
C				D.S.Briggs	May 28 1994
C	Now weights can be simulated with weights:
C	WT = ((1.0 + ZenithNoise)/( 1.0 + AtmosNoise))**2
C 	AtmosNoise = Tatm * (1.0 - EXP(-TAU/SIN(EL))) * EXP(TAU/SIN(EL))
C	ZenithNoise = Tatm * (1.0 - EXP(-TAU)) * EXP(TAU)
C	Tatm is the temperature of the atmosphere
C	TAU is the optical depth at the zenith
C
C	Zenith Observations will have a wt of 1.0
C	Only works when simulating an observation from scratch
C				M.A. Holdaway	June 14 1994
C	Removed low elevation approximation, now more accurate
C				M.A. Holdaway	June 28 1994
C	Added GainRef/GainExample feature.  (I seem to be a regular
C	around here too.)
C				D.S.Briggs	July 9 1994
C	Added time variable models.
C				D.S.Briggs	Nov 19 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSIM')
C
      REAL                      CELLSIZE(3), SHIFT(3)
      REAL		        HALIMITS(2), DEC, ELMIN, INTTIME
      REAL			PHRMS, GRMS, GDRIFT, NRMS
      REAL	        	PI, TAU, TATM, TREC
      REAL	       		HMIN, HMAX
      PARAMETER                 (PI=3.14159265358979)
      CHARACTER*(SYSMXNAM)	MODFILE, ANTFILE, MAPFILE, STOKES,
     $				VISFILE, CTIME, TELE, VISOBSFI,
     $   			GREFFILE, GEXFILE, GTMODE
      INTEGER		        NDUMMY, IMSIZE(3), SEED(2)
C
      REAL		FREQ, AUTOW, TIME, PIXPC(2), WORLDPC(2),TELDIAM,
     $   		WT(3), TIMEREF
      LOGICAL		ADDN, DOPB, DO3D, DOVISMOD, DOTIMEV,
     $   		MAPCRDS, ZERO
C
      CHARACTER*8	CTYPE(SYSMXDIM)
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     $   		CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM), OBSRA, OBSDEC
C
      DATA           HALIMITS  /-.1, .1/
      DATA           DEC       /50./
      DATA           ELMIN     /30./
      DATA           INTTIME   /60./
      DATA           IMSIZE    /512, 512, 1/
      DATA           CELLSIZE  /1., 1., 1./
      DATA           SHIFT     /0., 0., 0./ 
C
      DOUBLE PRECISION	DATFGETD, CRDGRA, CRDGDEC
      REAL		CRDGFREQ
      LOGICAL		DATEXIST
      INTEGER		STRLEN
C==================================================================
      CALL MSGWELCO ('I make simulated array data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETC ('VisObs', VISOBSFI, 1, NDUMMY)
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETL ('DoVisModel', DOVISMOD, 1, NDUMMY)
      CALL USRGETL ('DoTimeVar', DOTIMEV, 1, NDUMMY)
      CALL USRGETR ('TimeRef', TIMEREF, 1, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETC ('Map', MAPFILE, 1, NDUMMY)
      CALL USRGETL ('MapCoords', MAPCRDS, 1, NDUMMY)
      CALL USRGETR ('HAlimits', HALIMITS, 2, NDUMMY)
      CALL USRGETR ('Dec', DEC, 1, NDUMMY)
      CALL USRGETR ('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR ('ELmin', ELMIN, 1, NDUMMY)
      CALL USRGETR ('Autow', AUTOW, 1, NDUMMY)
      CALL USRGETR ('INTtime', INTTIME, 1, NDUMMY)
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
      CALL USRGETC ('GainRef', GREFFILE, 1, NDUMMY)
      CALL USRGETC ('GainExample', GEXFILE, 1, NDUMMY)
      CALL USRGETC ('GTMode', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, GTMODE)
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
      IF (DOTIMEV) THEN
         IF (.NOT.DOVISMOD) THEN
            CALL MSGPUT ('You must use visibility plane modelling',
     $         ' with time variable models!','E')
            GO TO 999
         END IF
      END IF
C
      IF (GTMODE.EQ.' ') GTMODE = 'NONE'
      IF ((GTMODE.NE.'AMP').AND.(GTMODE.NE.'PHASE')
     $    .AND.(GTMODE.NE.'AMPPHASE').AND.(GTMODE.NE.'NONE')) THEN
         CALL MSGPUT ('Unrecognized GTMODE','E')
         GO TO 999
      END IF
C
C If we have a visibility template, use it.
C
      IF (VISOBSFI.NE.' ') THEN
         CALL MSGPUT ('Simulating existing dataset','I')
         CALL VISGET ('Vis', VISOBSFI, STOKES, '*', ' ')
         CALL ARRSETCO ('Vis/OBS/I/VIS', 0.0, 0.0)
         IF (STOKES.EQ.'IV') CALL ARRSETCO ('Vis/OBS/V/VIS', 0.0, 0.0)
      ELSE
C
C Read the antennas file
C  
         CALL FILGETAN ('Antfile', ANTFILE)
         CALL DATPUTR ('Antfile', 'Tau', TAU, 1)
         CALL DATPUTR ('Antfile', 'Tatm', TATM, 1)
         CALL DATPUTR ('Antfile', 'Trec', TREC, 1)
C
         HMIN = MIN (HALIMITS(1), HALIMITS(2))
         HMAX = MAX (HALIMITS(1), HALIMITS(2))
         TIME = 0.0
C
C Now make empty data sets
C
         CALL MSGPUT ('Making initial datasets', 'I')
         CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX),
     1      DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(INTTIME), 
     $      AUTOW.NE.0.0, AUTOW, 'Vis')
C
C Reweight it if requested
C
         IF ((WT(1).NE.1.0).OR.(WT(2).NE.0.0).OR.(WT(3).NE.0.0))
     $      CALL VISRWTDT ('Vis', 'OBS/I', 'Antfile', WT)
C
      END IF
C
C Get Gain transfer files if we need them
C
      IF (GTMODE.NE.'NONE') THEN
         CALL VISGET ('GainRef', GREFFILE, STOKES, '*', ' ')
         CALL VISGET ('GainExamp', GEXFILE, STOKES, '*', ' ')
         IF (ERROR) GO TO 999
      END IF
C
      CALL MSGPUT ('Coordinates for visibility data', 'I')
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
            IF (DOTIMEV) THEN
               IF (.NOT.DATEXIST('Model/VEL')) THEN
                  CALL ERRREPOR (ERRFATAL, ROUTINE,
     $               'Model has no time information!')
                  GO TO 999
               END IF
            END IF
            CALL DATPUTR ('Model','TIMEREF', TIMEREF/24.0, 1)
            CALL MSGPUT ('Analytic model:','I')
            CALL MODLIST ('Model')
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
      IF (VISOBSFI.NE.' ') THEN
         IF (DATEXIST('Vis/OBSRA')) THEN
            OBSRA = DATFGETD ('Vis', 'OBSRA')
            OBSDEC = DATFGETD ('Vis', 'OBSDEC')
         ELSE
            OBSRA = CRDGRA('Vis/OBS/I')
            OBSDEC = CRDGDEC('Vis/OBS/I')
            CALL DATPUTD ('Vis', 'OBSRA', OBSRA, 1)
            CALL DATPUTD ('Vis', 'OBSDEC', OBSDEC, 1)
         END IF
         FREQ = CRDGFREQ('Vis/OBS/I')
      ELSE
         OBSRA = 0.D0
         OBSDEC = DBLE(DEC)
         IF (MAPCRDS) THEN
            IF (DATEXIST('Map/OBSRA'))
     $         OBSRA = DATFGETD ('Map', 'OBSRA')
            IF (DATEXIST('Map/OBSDEC'))
     $         OBSRA = DATFGETD ('Map', 'OBSDEC')
         END IF
         CALL DATPUTR ('Vis', 'REFDATE', 1950.0, 1)
         CALL DATPUTD ('Vis', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Vis', 'OBSDEC', OBSDEC, 1)
      END IF
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
C Transfer Calibration if desired
C
      IF (GTMODE.NE.'NONE') THEN
         MESSAGE = 'Transferring Gains (Mode = ' //
     $      GTMODE(1:STRLEN(GTMODE)) // ')'
         CALL MSGPUT (MESSAGE, 'I')
         IF (STOKES.EQ.'I') THEN
            CALL GAITRANS ('Vis/OBS/I', 'GainExamp/OBS/I',
     $         'GainRef/OBS/I', 'Vis/OBS/I', GTMODE)
         ELSE IF (STOKES.EQ.'IV') THEN
            CALL GAITRANS ('Vis/OBS/I', 'GainExamp/OBS/I',
     $         'GainRef/OBS/I', 'Vis/OBS/I', GTMODE)
            CALL GAITRANS ('Vis/OBS/V', 'GainExamp/OBS/V',
     $         'GainRef/OBS/V', 'Vis/OBS/V', GTMODE)
         END IF
      END IF
C
C Change STOKES
C
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
