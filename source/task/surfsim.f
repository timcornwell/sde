C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)surfsim.f	1.6	 5/13/91
C
      SUBROUTINE SDEMAIN
C
CD Program to make uv mosaic data sets for simulated array data
C
C Audit trail:
C	Added SDETYPE specification
C				T.J.Cornwell	Feb 3 1989
C	Added data corruption option 
C				R.Braun 	Jul 9 1989
C	Added global pointing errors (same errors for each ant per
C	pointing)
C				R.Braun		Jul 9 1989
C	Added individual antenna pointing errors through
C	SIMPE and IMGDFTPE
C	A note on pointing errors:  The GLOBAL Pointing Errors
C	(same pointing error for all antennas in each  pointing)
C	have "G" at the end and are accomplished by misslabeling
C	the OBSRA, OBSDEC; antenna independent errors are
C	accomplished via the pseudo-FT IMGDFTPE
C				M.A.Holdaway	Sep 19 1989
C	Even more extensive pointing error simulation.
C	Also, the data are created in the manner in which the
C	observations would actually be made: scanning through
C	all pointings NUMINT times.  This requires that we
C	create sub-directories such as "PC{n}I{m}", and later
C	regroup for all integration times {m} into "PC{n}".
C	Most realistic option.  This will increase the time for
C	the FFT case, but not very much for the DFT case.
C	WARNING!!!  If any node is added to the VIS,
C	you will need to go into VISREORG to transfer it there
C	too.
C				M.A.Holdaway	Dec 11 1989
C	Added DFT logical
C				T.J.Cornwell	April 17 1990
C	Option to make pointings on equally spaced sin projection grid
C	rather than equally spaced SKY RA and DEC grid
C				M.A.Holdaway	May 9 1990
C	Now complex fluctuations in antenna signals.
C				M.A.Holdaway	May 22 1990
C	Added  support for general Airy telescope
C				T.J.Cornwell	May 29 1990
C	Added primary beam corrupting capability
C				M.A.Holdaway	Sept 12 1990
C	Fixed VISPUT and renamed to mossim
C				T.J.Cornwell    Nov 1 1990
C	Removed spurious argument STOKES to SIMUV
C				T.J.Cornwell    Nov 2 1990
C	Fixed the pointing spacing for arbitrary antenna size TELDIAM
C				M.A.Holdaway	Nov 11 1990
C	Added Beam Switching
C				M.A.Holdaway	Jan 10 1990
C	Changed to REAL variables for FREQ, WAVE, HALIMITS, DEC, ELMIN,
C	INTTIME, BEAMTHROW
C				T.J.Cornwell    Feb 4 1991
C	In all calls to SIMUV, FREQ was passed erroneously as REAL.
C	Also fixed SNGL operation on FREQ, and added comma to FORMAT
C	statement (picked up by IBM)
C				T.J.Cornwell    Feb  10 1991
C***********************************************************************
C	Previous comments refer to MOSSIM.  From here down, SURFSIM is born
C	SURFSIM reads in a Voltage Pattern for each antenna.  The VP is
C	rotated as per the paralactic angle and applied to the image
C	Visibilities = 	VISDEGRID ( FFT (MODEL . VP1 . VP2* ) )
C
C	The VP's are specified by a single name, VPNAME.  It is assumed
C	that the disk files have names VPNAME//STRINT(IANT), for IANT=1, NANT
C
C	VP's are made by SURFMAKE, which likes to run on the CONVEXES so it
C	can do 1K x 1K FFTS.  The VP's need to be HGEOM'ed for two reasons:
C	1: Cell size is much larger than the image to which they are applied
C	2: Need to be rotated by PARANGLE
C
C	Removed PB, PBLEVEL, as they were not pertinent
C
C				M.A.Holdaway	Feb 28 1991
C
C	Implemented VP averaging to get an effective PB, which
C	is attached to the MOSAIC DATABASE
C				M.A.Holdaway	March 8 1991
C	Fixed some dum bugs the IBMDEMO caught
C				M.A.Holdaway	March 28 1991
C	Added input parameter SAME, which will only try to read
C	in ONE VP
C				M.A.Holdaway	March 29 1991
C	Pass SEED down to VISCORRU
C				M.A.Holdaway	May 1 1991
C	Pass Starting TIME to SIMUV
C				M.A.Holdaway	May 12 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SURFSIM')
C
      REAL                      CELLSIZE(3), SHIFT(3), AUTOW
      REAL                      PHRMS, NRMS, GRMS, GDRIFT, TELDIAM
      REAL			GLPNT(2), INPNT(2), DRPNT(2), RANPNT(2)
      REAL		        HALIMITS(2), DEC, ELMIN, INTTIME
      DOUBLE PRECISION          HAMIN, HAMAX, DEL, DEL1, PI
      DOUBLE PRECISION          HMIN, HMAX, HDEL
      DOUBLE PRECISION          OBSRA, OBSDEC, PRA, PDEC, BT(2)
      DOUBLE PRECISION		BTRA, BTDEC
      REAL			DRIFT(2)
      PARAMETER                 (PI=3.14159274101257)
      CHARACTER*(SYSMXNAM)	VISFILE, MODFILE, ANTFILE, MAPFILE,
     1				MOSFILE, TELE, VPNAME, VPNAME2, VPNAME3
      CHARACTER*(SYSMXNAM)	VIS, VISROOT, OVISROOT, STRM2
      CHARACTER*6               STRINT
      CHARACTER*10		STRREAL
      CHARACTER*4		STOKES
      INTEGER		        NDUMMY, IMSIZE(3), NPC, NED, IVIS, SEED
      LOGICAL			DFT, SAME
      INTEGER                   IX, IY, NUMINT, INUMINT, I, VIP
      LOGICAL			AUTOC, ADDN, PROJECT, PBAVE
C
      REAL		FREQ, WAVE, PARANGLE, TIME
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NANT
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     1			CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
      DOUBLE PRECISION	SLAT, HAVE
      REAL		AZERR, ELERR, RAERR, DECERR, GDERR
C
      CHARACTER*(SYSMXNAM)	STRRMBL
C
      DATA           HALIMITS  /-.1, .1/
      DATA           DEC       /50./
      DATA           ELMIN     /30./
      DATA           INTTIME   /60./
      DATA           IMSIZE    /512, 512, 1/
      DATA           CELLSIZE  /1., 1., 1./
      DATA           SHIFT     /0., 0., 0./ 
      DATA	     BTRA /0.0/
      DATA	     BTDEC /0.0/
C==================================================================
      CALL MSGWELCO ('I make simulated mosaic array data')
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
      CALL USRGETR('BeamThrow', BT, 2, NDUMMY)
      CALL USRGETR('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR('ELmin', ELMIN, 1, NDUMMY)
      CALL USRGETR('INTtime', INTTIME, 1, NDUMMY)
      CALL USRGETI('NED', NED, 1, NDUMMY)
      CALL USRGETL ('AutoC', AUTOC, 1, NDUMMY)
      CALL USRGETR ('AutoW', AUTOW, 1, NDUMMY)
      CALL USRGETL ('ADDN', ADDN, 1, NDUMMY)
      CALL USRGETR ('PHRMS', PHRMS, 1, NDUMMY)
      CALL USRGETR ('NRMS', NRMS, 1, NDUMMY)
      CALL USRGETR ('GRMS', GRMS, 1, NDUMMY)
      CALL USRGETR ('GDRIFT', GDRIFT, 1, NDUMMY)
      CALL USRGETR ('GLPNT', GLPNT, 2, NDUMMY)
      CALL USRGETR ('ORPNT', INPNT, 2, NDUMMY)
      CALL USRGETR ('DRPNT', DRPNT, 2, NDUMMY)
      CALL USRGETR ('RANPNT',RANPNT,2, NDUMMY)
      CALL USRGETL ('Sinproj', PROJECT , 1, NDUMMY)
      CALL USRGETL ('DFT', DFT, 1, NDUMMY)
      CALL USRGETC ('Telescope', TELE, 1, NDUMMY)
      CALL USRGETR ('Teldiam', TELDIAM, 1, NDUMMY)
      CALL USRGETC ('VPname', VPNAME, 1, NDUMMY)
      CALL USRGETL ('SameVP', SAME, 1, NDUMMY)
      CALL USRGETL ('PBAVE', PBAVE, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Mosaic', MOSFILE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
      VIP = 1
      SEED = 12345
C
C Read the antennas filen
C  
      CALL FILGETAN ('Antfile', ANTFILE)
C
C Read in all Voltage Patterns
C
      CALL DATCREAT ('VP')
      CALL DATGETI ('Antfile', 'NANT', NANT, 1, NDUMMY)
      CALL DATPUTI ('VP', 'NANT', NANT, 1)
      IF (SAME) THEN
         VPNAME2 = STRRMBL (VPNAME(1:40)//'1.SDE')
         CALL FILIMGGE ('VP/ANT1', VPNAME2, ' ')
         DO 3 I = 2, NANT
            CALL IMGCLONE ('VP/ANT1', 'VP/ANT'//STRINT(I))
            CALL ARRCOPY ('VP/ANT1', 'VP/ANT'//STRINT(I))
 3       CONTINUE
      ELSE
         IF (ERROR) GOTO 999
         DO 4 I = 1, NANT
            VPNAME2 = STRRMBL (VPNAME(1:40)//STRINT(I)//'.SDE')
            CALL FILIMGGE ('VP/ANT'//STRINT(I), VPNAME2, ' ')
C
C If it croaked, try to read the next one in its place
C
            IF (ERROR) THEN
               CALL ERRCANCE
               VPNAME2 = STRRMBL (VPNAME(1:40)//STRINT(I+1)//'.SDE')
               CALL FILIMGGE ('VP/ANT'//STRINT(I), VPNAME2, ' ')
               IF (ERROR) GOTO 999
               WRITE (MESSAGE, 9192) I, I+1
 9192          FORMAT ('VP',I3,' was missing, replaced it by VP',I3)
               CALL MSGPUT (MESSAGE, 'W')
            ENDIF
            CALL DATPUTL  ('VP/ANT'//STRINT(I), 'ROTATABLE', .TRUE., 1)
 4       CONTINUE
      ENDIF
C
C Clarify pointing error model
C
      CALL MSGPUT ('POINTING ERROR MODEL (ARCSECONDS)', 'I')
      MESSAGE='GLOBAL POINTING ERRORS : AZ '//STRREAL(GLPNT(1),3)//
     $   '  EL '//STRREAL(GLPNT(2),3)
      CALL MSGPUT (MESSAGE, 'I')
      MESSAGE='INITIAL POINTING SPREAD: AZ '//STRREAL(INPNT(1),3)//
     $   '  EL '//STRREAL(INPNT(2),3)
      CALL MSGPUT (MESSAGE, 'I')
      MESSAGE='DRIFT IN POINTING      : AZ '//STRREAL(DRPNT(1),3)//
     $   '  EL '//STRREAL(DRPNT(2),3)
      CALL MSGPUT (MESSAGE, 'I')
      MESSAGE='RANDOM POINTING ERRORS : AZ '//STRREAL(RANPNT(1),3)//
     $   '  EL '//STRREAL(RANPNT(2),3)
      CALL MSGPUT (MESSAGE, 'I')
      DO 8 I = 1, 2
         GLPNT (I) = GLPNT (I)/3600.
         INPNT (I) = INPNT (I)/3600.
         DRPNT (I) = DRPNT (I)/3600.
         RANPNT(I) = RANPNT(I)/3600.
 8    CONTINUE
C
C for multiple integrations on source, scan through all pointings 
C a number of times
C
      NPC = (NED*2+1)**2
      HAMIN = MIN (HALIMITS(1), HALIMITS(2))
      HAMAX = MAX (HALIMITS(1), HALIMITS(2))
      HDEL = (HAMAX - HAMIN) / (NPC*1.D0)
      NUMINT = MAX0(NINT(HDEL*3600./INTTIME),1)
      HMIN = HAMIN
      HDEL = HDEL/FLOAT(NUMINT)
      HMAX = HMIN + HDEL
      TIME = 0.0
C Hdel is now the time for one integration
C
C Now make an empty data set
C
      STOKES = 'I'
      INUMINT = 1
      CALL SIMUV (DBLE(FREQ), 'Antfile', HMIN, HMAX, DBLE(DEC), 
     1   DBLE(ELMIN), DBLE(TIME), DBLE(INTTIME), AUTOC, AUTOW, 'IVis')
      CALL DATPUTC ('IVis', 'TELESCOP', TELE, 1)
      IF(TELE(1:4).EQ.'AIRY') THEN
         CALL DATPUTR ('IVis', 'TELDIAM', TELDIAM, 1)
      END IF
C
C Read the model file or image
C
      IF (MODFILE.NE.' ') THEN
         CALL FILGETMO('Model', MODFILE)
C
C Make an image
C
         CALL IMGMAKE ('IVis/OBS/I', CELLSIZE, IMSIZE, SHIFT, 'R',
     $      'Map')
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
C
C Set up model for mosaic sampling
C
      CALL CRDGET ('Map', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $   CROTA)
      NAX = 3
      CRVAL(1) = 0.D0
      CRVAL(2) = DBLE(DEC)
      CRVAL(3) = DBLE(FREQ)
      CTYPE(3) = 'FREQ'
      NAXIS(3) = 1
      CRPIX(3) = 1.
      CDELT(3) = 0.1 * FREQ
      CROTA(3) = 0.
      CALL CRDPUT ('Map', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $   CROTA)
      CALL DATPUTC ('Map', 'TELESCOP', TELE, 1)
      IF(TELE(1:4).EQ.'AIRY') THEN
         CALL DATPUTR ('Map', 'TELDIAM', TELDIAM, 1)
      END IF
      CALL IMGCLONE ('Map', 'PBMap')
C
C Make and fill the mosaic data set
C
      VISROOT  = 'Vis'
      OVISROOT = 'Temp'
      CALL DATCREAT (VISROOT)
      CALL DATCREAT (OVISROOT)
C
      WAVE = 3E8/FREQ
      IF (TELE(1:3) .EQ. 'MMA') THEN
         DEL = 0.5 * (180.0/PI) * ( WAVE / 7.5)
      ELSE
         DEL = 0.5 * (180.0/PI) * ( WAVE / TELDIAM)
      ENDIF
      DEL1 = DEL * 3600.0
      WRITE (MESSAGE, 1017) DEL1
 1017 FORMAT ('Pointings separated by ',F12.3, ' arcseconds ')
      CALL MSGPUT(MESSAGE, 'I')
      IVIS = 0
      MESSAGE = 'We will make '//STRINT(NUMINT)//
     $   ' integration intervals'
      CALL MSGPUT (MESSAGE, 'I')
      CALL DATGETD ('Antfile',  'SITELAT', SLAT, 1, NDUMMY)
      IF (ERROR) GOTO 999
C
C Make the "average" primary beam, attach to VISROOT/PB directory
C
      IF (PBAVE) THEN
         CALL IMGDOUBL ('Map', 'Map2')
         CALL IMGPBAVE (VISROOT, 'Map2', 'VP', TELDIAM)
         CALL DATDELET ('Map2')
      ENDIF
C
C GRUNT!  Lets kick some VIS!
C
      DO 30 INUMINT = 1, NUMINT
       IVIS = 0
       MESSAGE = 'Integration number '//STRINT(INUMINT)
       CALL MSGPUT (MESSAGE, 'I')
       DO 20 IY = -NED, NED
         DO 10 IX = -NED, NED
            IF (PROJECT) THEN
               PRA = CRVAL(1) + IX * DEL 
               PDEC = CRVAL(2) + IY * DEL
               CALL CRDINSIN (CRVAL(1), CRVAL(2), PRA, PDEC, 
     $            OBSRA, OBSDEC)
            ELSE
               OBSDEC = CRVAL(2) + IY * DEL
               OBSRA = CRVAL(1) + IX * DEL / COS(CRVAL(2)*PI/180.D0)
            ENDIF
            IVIS = IVIS + 1
            VIS = STRM2( OVISROOT, 
     $         'PC'//STRINT(IVIS)//'I'//STRINT(INUMINT))
            VIS = STRRMBL (VIS)
            MESSAGE = 'Data go to '//VIS
            CALL MSGPUT (MESSAGE, 'I')
            CALL SIMUV (DBLE(FREQ), 'Antfile', HMIN, HMAX, DBLE(DEC), 
     1         DBLE(ELMIN), DBLE(TIME), DBLE(INTTIME), AUTOC, 
     $         AUTOW, VIS)
            TIME = TIME + INTTIME/86400.
            HMIN = HMIN + HDEL
            HMAX = HMAX + HDEL
            IF (HMAX.GT.(HAMAX+0.01D0)) THEN
               HMIN = HAMIN 
               HMAX = HMIN + HDEL 
            END IF
            HAVE = (HMIN + HMAX)/2.D0
C
            IF (NPC*NUMINT .EQ. 1) THEN
               DRIFT(1) = 0.
               DRIFT(2) = 0.
            ELSE
               DRIFT(1) = DRPNT(1) * FLOAT( (IVIS-1) +
     $            (INUMINT-1)*NPC )/ FLOAT(NPC*NUMINT - 1)
               DRIFT(2) = DRPNT(2) * FLOAT( (IVIS-1) +
     $            (INUMINT-1)*NPC )/ FLOAT(NPC*NUMINT - 1)
            ENDIF
            CALL SIMPE ('Antfile', HMIN, HMAX, HAVE, OBSDEC, 
     $         DBLE(INTTIME), VIS, GLPNT(1), GLPNT(2), INPNT(1), 
     $         INPNT(2), DRIFT(1),DRIFT(2), RANPNT(1), RANPNT(2))
C
            CALL DATPUTD (VIS, 'OBSRA', OBSRA, 1)
            CALL DATPUTD (VIS, 'OBSDEC', OBSDEC, 1)
            CALL DATPUTC (VIS, 'TELESCOP', TELE, 1)
            IF(TELE(1:4).EQ.'AIRY') THEN
               CALL DATPUTR (VIS, 'TELDIAM', TELDIAM, 1)
            END IF
            CALL DATPUTD ('Map', 'OBSRA', OBSRA, 1)
            CALL DATPUTD ('Map', 'OBSDEC', OBSDEC, 1)
            IF (BT(1) .NE. 0.D0 .OR. BT(2) .NE. 0.D0) THEN
               CALL SIMAERD (OBSDEC, HAVE, SLAT, BT(1), BT(2),
     $            BTRA, BTDEC)
               BTRA  = BTRA  + OBSRA
               BTDEC = BTDEC + OBSDEC
               CALL DATPUTD ('Map', 'THROWRA', BTRA, 1)
               CALL DATPUTD ('Map', 'THROWDEC', BTDEC, 1)
               CALL DATPUTD (VIS, 'THROWRA', BTRA, 1)
               CALL DATPUTD (VIS, 'THROWDEC', BTDEC, 1)               
            ENDIF
            IF (ERROR) GO TO 999
            WRITE (MESSAGE, 1100) IX, IY
 1100       FORMAT ('Creating pointing (',I3,',',I3,')')
            CALL MSGPUT (MESSAGE, 'I')
C
            CALL DATGETR (VIS, 'PARANGLE', PARANGLE, 1, NDUMMY)
            CALL DATPUTR ('Map', 'PARANGLE', PARANGLE, 1)
            CALL MSGPUT ('Transforming model, DFT', 'I')
            CALL IMGDFTVP (VIS, 'OBS/I', 'Map', 'VP')
C
            IF (ADDN) THEN
               IF (NPC*NUMINT .EQ. 1) THEN
                  GDERR = 0.
               ELSE
                  GDERR = GDRIFT * FLOAT( (IVIS-1) +
     $               (INUMINT-1)*NPC )/ FLOAT(NPC*NUMINT - 1)
               ENDIF
               CALL VISCORRU (VIS, PHRMS, GRMS, GDRIFT, NRMS, 
     $            SEED, 'OBS/I', 'OBS/I')
            END IF
  10     CONTINUE
  20   CONTINUE
  30  CONTINUE 
C
C Put VIS back into 'Vis/PC'//STRINT(IVIS) form
C
      CALL VISREORG (VISROOT, OVISROOT, NPC, NUMINT)
      IF (MOSFILE.NE.' ') THEN
         CALL MSGPUT ('Writing new mosaic file as SDE file', 'I')
         CALL DATPUTI ('Vis', 'NPC', IVIS, 1)
         CALL DATSETTP ('Vis', 'VISMOSAIC')
         CALL VISMOSPU ('Vis', MOSFILE)
      ENDIF
      IF (VISFILE.NE.' ') THEN
         CALL MSGPUT ('Writing one pointing only', 'I')
         CALL DATPUTD ('Vis/PC'//STRINT(VIP), 'OBSRA', 0.0D0, 1)
         CALL DATPUTD ('Vis/PC'//STRINT(VIP), 'OBSDEC', DEC, 1)
         CALL DATSETTP ('Vis/PC'//STRINT(VIP), 'VIS')
         CALL HISINPUT ('Vis/PC'//STRINT(VIP))
         CALL VISPUT ('Vis/PC'//STRINT(VIP), VISFILE, 
     $      'OBS', 'I', '*', ' ')
      END IF
C
 999  CONTINUE
      END




