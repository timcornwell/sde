C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to make uv mosaic data sets for simulated array data
C
CS Arguments: CALL SDEMAIN
CA Audit trail:
CA	Added SDETYPE specification
CA				T.J.Cornwell	Feb 3 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'AMSIM')
C
      REAL                      CELLSIZE(3), SHIFT(3), AUTOW
      DOUBLE PRECISION          HALIMITS(2), DEC, ELMIN, INTTIME
      DOUBLE PRECISION          HAMIN, HAMAX, DEL, PI
      DOUBLE PRECISION          HMIN, HMAX, HDEL 
      DOUBLE PRECISION          OBSRA, OBSDEC 
      PARAMETER                 (PI=3.14159274101257)
      CHARACTER*(SYSMXNAM)	VISFILE, MODFILE, ANTFILE, MAPFILE,
     1				MOSFILE, TELE
      CHARACTER*(SYSMXNAM)	VIS 
      CHARACTER*6               STRINT
      INTEGER		        NDUMMY, IMSIZE(3), NPC, NED, IVIS
      INTEGER                   IX, IY, NDAY, NPDAY
      LOGICAL			PB, AUTOC
C
      DOUBLE PRECISION	FREQ, WAVE
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     1			CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
C
      DATA           HALIMITS  /-.1, .1/
      DATA           DEC       /50./
      DATA           ELMIN     /30./
      DATA           INTTIME   /60./
      DATA           IMSIZE    /512, 512, 1/
      DATA           CELLSIZE  /1., 1., 1./
      DATA           SHIFT     /0., 0., 0./ 
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
      CALL USRGETD('HAlimits', HALIMITS, 2, NDUMMY)
      CALL USRGETD('Dec', DEC, 1, NDUMMY)
      CALL USRGETD('Freq', FREQ, 1, NDUMMY)
      CALL USRGETD('ELmin', ELMIN, 1, NDUMMY)
      CALL USRGETD('INTtime', INTTIME, 1, NDUMMY)
      CALL USRGETI('NED', NED, 1, NDUMMY)
      CALL USRGETI('NDay', NDAY, 1, NDUMMY)
      CALL USRGETL ('AutoC', AUTOC, 1, NDUMMY)
      CALL USRGETR ('AutoW', AUTOW, 1, NDUMMY)
      CALL USRGETL ('PB', PB, 1, NDUMMY)
      CALL USRGETC('Telescope', TELE, 1, NDUMMY)
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Mosaic', MOSFILE, 1, NDUMMY)
C
C Read the antennas file
C  
      CALL FILGETAN ('Antfile', ANTFILE)
C
      NPC = (NED*2+1)**2
      NPDAY = NPC / NDAY
      HAMIN = MIN (HALIMITS(1), HALIMITS(2))
      HAMAX = MAX (HALIMITS(1), HALIMITS(2))
      HDEL = (HAMAX - HAMIN) / (NPDAY*1.D0)
      HMIN = HAMIN
      HMAX = HMIN + HDEL
C
C Now make an empty data set
C
      CALL SIMUV (FREQ, 'Antfile', HMIN, HMAX, DEC, ELMIN, 
     1   INTTIME, AUTOC, AUTOW, 'IVis')
      CALL DATPUTC ('IVis', 'TELESCOP', TELE, 1)
C
C Read the model file or image
C
      IF (MODFILE.NE.' ') THEN
         CALL FILGETMO('Model', MODFILE)
C
C Make an image
C
         CALL IMGMAKE ('IVis/OBS/I', CELLSIZE, 
     1                               IMSIZE, SHIFT, 'R', 'Map')
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
      CALL CRDGET ('Map', NAX, CTYPE, NAXIS, CRVAL, 
     1                             CRPIX, CDELT, CROTA)
      NAX = 3
      CRVAL(1) = 0.D0
      CRVAL(2) = DEC
      CRVAL(3) = FREQ
      CTYPE(3) = 'FREQ'
      NAXIS(3) = 1
      CRPIX(3) = 1.
      CDELT(3) = 0.1 * FREQ
      CROTA(3) = 0.
      CALL CRDPUT ('Map', NAX, CTYPE, NAXIS, CRVAL, 
     1                             CRPIX, CDELT, CROTA)
      CALL DATPUTC ('Map', 'TELESCOP', TELE, 1)
      CALL IMGCLONE ('Map', 'PBMap')
C
C Make and fill the mosaic data set
C
      CALL DATCREAT ('Vis')
      CALL DATSETTP ('Vis', 'VISMOSAIC')
C
      WAVE = 3E8/FREQ
      DEL = 0.5D0 * (180.D0/PI) * (1.22D0 * WAVE / 7.5D0)
      IVIS = 0
      DO 20 IY = -NED, NED
         OBSDEC = CRVAL(2) + IY * DEL
         DO 10 IX = -NED, NED
            OBSRA = CRVAL(1) + IX * DEL / COS(CRVAL(2)*PI/180.D0)
            IVIS = IVIS + 1
            VIS = 'Vis/PC'//STRINT(IVIS)
            CALL SIMUV (FREQ, 'Antfile', HMIN, HMAX, DEC, 
     1                          ELMIN, INTTIME, AUTOC, AUTOW, VIS)
            HMIN = HMIN + HDEL
            HMAX = HMAX + HDEL
            IF (HMAX.GT.(HAMAX+0.01D0)) THEN
               HMIN = HAMIN 
               HMAX = HMIN + HDEL 
            END IF
            CALL DATPUTC (VIS, 'TELESCOP', TELE, 1)
            CALL DATPUTD (VIS, 'OBSRA', OBSRA, 1)
            CALL DATPUTD (VIS, 'OBSDEC', OBSDEC, 1)
            IF (ERROR) GO TO 999
            WRITE (MESSAGE, 1100) IX, IY
 1100       FORMAT ('Creating pointing (',I2,','I2')')
            CALL MSGPUT (MESSAGE, 'I')
            CALL DATPUTD ('Map', 'OBSRA', OBSRA, 1)
            CALL DATPUTD ('Map', 'OBSDEC', OBSDEC, 1)
            IF (PB) THEN
               CALL IMGPB ('Map', 'PBMap', 'APPLY')
               CALL IMGGRIDC ('PBMap', 'PBMap', 'CORRECT')
            ELSE
               CALL IMGGRIDC ('Map', 'PBMap', 'CORRECT')
            END IF
            CALL IMGFFT ('PBMap', 'Mapvis')
            CALL MSGPUT ('Transforming model', 'I')
            CALL VISDEGRI (VIS, 'OBS/I', 'Mapvis')
  10  CONTINUE
  20  CONTINUE
C
      IF (MOSFILE.NE.' ') THEN
         CALL MSGPUT ('Writing new mosaic file as SDE file', 'I')
         CALL DATPUTI ('Vis', 'NPC', IVIS, 1)
         CALL VISMOSPU ('Vis', MOSFILE)
      ELSEIF (VISFILE.NE.' ') THEN
         CALL MSGPUT ('Writing first pointing only', 'I')
         CALL VISPUT ('Vis/PC1', VISFILE, 'OBS/I', '*', '*')
      END IF
C
 999  CONTINUE
      END
