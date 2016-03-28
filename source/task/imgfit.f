C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgfit.f	1.2    6/14/94
C
      SUBROUTINE SDEMAIN
C
CD Program to fit a gaussian to an image
C
C This is a seriously simple minded image fitting program.  Maybe someday
C we'll generalize it, though for more complicated problems gfit +
C gaussfit is probably the tool of choice.
C
C Since this uses the guts of the beam fitting routines, ignore a few
C odd messages about non unity peak and the like.
C
C Audit trail:
C	Original version
C				D.S.Briggs	March 20 1994
C	Beamfit selecion tweaked
C				D.S.Briggs	June 14 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGFIT')
C
      CHARACTER*(SYSMXNAM)	IMGFILE, BEAMFILE
      REAL		BEAM(4), FIT(4), DCON(4), TAPER(4),
     $   		FITAMP, FITFLUX, FITDX, FITDY
      INTEGER		NDUMMY
      LOGICAL		DODECON
      CHARACTER*(SYSMXNAM)	BUNIT, FITALG
C
      LOGICAL		DATEXIST
      REAL		DATFGETR
C==================================================================
C
      CALL MSGWELCO ('I fit a Gaussian to an image')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('Beamfile', BEAMFILE, 1, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
      CALL FILIMGGE ('Image', IMGFILE, ' ')
      BUNIT = ' '
      IF (DATEXIST('Image/BUNIT'))
     $   CALL DATGETC ('Image', 'BUNIT', BUNIT, 1, NDUMMY)
      IF (BUNIT.EQ.'JY/BEAM') THEN
         TAPER(1) = DATFGETR ('Image','BMAJ') * 3600.0
         TAPER(2) = DATFGETR ('Image','BMIN') * 3600.0
         TAPER(3) = DATFGETR ('Image','BPA')
         TAPER(4) = 0.0
         CALL IMGPB2P ('Image', TAPER, 'Image')
      END IF
C
      DODECON = ((BEAM(1).NE.0.0).OR.(BEAMFILE.NE.' '))
      IF (DODECON) CALL FILBEMGE (BEAMFILE, BEAM, FITALG, 'BEAM')
      IF (ERROR) GO TO 999
C
      CALL IMGGSFIT ('Image')
      IF (ERROR) GO TO 999
C
      FIT(1) = DATFGETR ('Image', 'GMAJ')
      FIT(2) = DATFGETR ('Image', 'GMIN')
      FIT(3) = DATFGETR ('Image', 'GPA')
      FIT(4) = DATFGETR ('Image', 'GZ')
      FITAMP = DATFGETR ('Image', 'GAMP')
      FITFLUX = DATFGETR ('Image', 'GFLUX')
      FITDX = DATFGETR ('Image', 'GDX')
      FITDY = DATFGETR ('Image', 'GDY')
C
      IF (DODECON) THEN
         TAPER(1) = -BEAM(1)
         TAPER(2) = -BEAM(2)
         TAPER(3) = BEAM(3)
         TAPER(4) = 0.0
         CALL UTLGCONV (FIT, TAPER, DCON)
      END IF
C
      CALL MSGPUT (' ','I')
      CALL MSGPUT ('Fitted Flux, Amplitude, DX, DY','I')
      WRITE (MESSAGE, 1000) FITFLUX, FITAMP, FITDX, FITDY
 1000 FORMAT (1P4E13.5)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1010) FIT
 1010 FORMAT ('Fitted size = ',1P2E13.5,0PF11.3,E13.5)
      CALL MSGPUT (MESSAGE, 'I')
C
      IF (DODECON) THEN
         WRITE (MESSAGE, 1020) DCON
 1020    FORMAT ('Deconvolved size = ',1P2E13.5,0PF11.3,E13.5)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
 999  CONTINUE
      END
