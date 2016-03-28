C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)si.f	1.4    5/1/91
C
      SUBROUTINE SDEMAIN
C
CD Program to make images for the solar interferometer
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed calling of VISCORRU
C				T.J.Cornwell	April 7 1991
C	Pass SEEDV down to VISCORRU
C				M.A.Holdaway	May 1 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SI')
C
      CHARACTER*(SYSMXNAM)	MODFILE, IMGFILE, DRTFILE, PSFFILE,
     $			TPSFFILE
      INTEGER		NDUMMY, IMSIZE(3), DIR, INTNDX, NSEL, SEED,
     $   		SEEDV
      REAL		CELLSIZE(3), SHIFT(3), PHASE, NRMS, DATFGETR
      REAL		PHOTONS, SCALE, MIND, BEAM(4), TIME(2)
      REAL		UVLIMITS(2), CHISQ, BACK
      LOGICAL		DFT
C
#include		"si.h"
C
      DATA	IMSIZE		/128, 128, 1/
      DATA	CELLSIZE	/1.0, 1.0, 1.0/
      DATA	NRMS		/0.0/
      DATA	UVLIMITS	/0.0, 1E10/
C==================================================================
      CALL MSGWELCO ('I make SI data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETR ('Phase', PHASE, 1, NDUMMY)
      CALL USRGETR ('Photons', PHOTONS, 1, NDUMMY)
      CALL USRGETR ('Background', BACK, 1, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('TPSF', TPSFFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETR ('Mind', MIND, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETR ('Wave', WAVE, 1, NDUMMY)
      CALL USRGETR ('Antloc', ANTLOC, NANT, NDUMMY)
      CALL USRGETR ('D', TELDIAM, 1, NDUMMY)
      CALL USRGETR ('Tint', TINT, 1, NDUMMY)
      CALL USRGETR ('Thint', THINT, 1, NDUMMY)
      CALL USRGETR ('Theta', THMAX, 1, NDUMMY)
      CALL USRGETL ('DFT', DFT, 1, NDUMMY)
      FREQ = 3.0E8 / WAVE
      BAND = 0.1 * FREQ
      NUMINT = 1 + THMAX/THINT
      SEEDV = 87601
C
      WRITE (MESSAGE,1200) ANTLOC
 1200 FORMAT ('Antenna locations =',4(1X,F7.3),' meters')
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE,1210) TELDIAM
 1210 FORMAT ('Antenna diameter = ', F7.3,' meters')
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE,1220) NUMINT
 1220 FORMAT ('Number of integrations = ', I4)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE,1230) THINT, THMAX
 1230 FORMAT ('Step in angle = ', F7.2, ', Maximum angle = ',F7.2,
     $   ' degrees')
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE,1240) WAVE*1E6
 1240 FORMAT ('Wavelength = ',F8.3,' microns')
      CALL MSGPUT (MESSAGE, 'I')
      IF  (DFT) THEN
         CALL MSGPUT ('Using DFT for transforms', 'I')
      ELSE
         CALL MSGPUT ('Using FFT for transforms', 'I')
      END IF
C
C First make the data base and corrupt it
C
      CALL MSGPUT ('Making visibility data', 'I')
      CALL SIUV ('Point')
      CALL VISPOINT ('Point', 'OBS/I', 'MOD/I', 1.0)
      CALL VISCORRU ('Point', PHASE, 0.0, 0.0, NRMS, SEEDV,
     $   'MOD/I', 'OBS/I')
      IF (ERROR) GO TO 999
C
C Now get model
C
      CALL MSGPUT ('Making model image', 'I')
      IF (IMGFILE.NE.' ') THEN
         CALL FILIMGGE ('Image', IMGFILE, ' ')
         CALL MSGPUT ('Coordinates of model image:', 'I')
         CALL CRDLIST ('Image')
      ELSE IF (MODFILE.NE.' ') THEN
         CALL IMGMAKE ('Point/OBS/I', CELLSIZE, IMSIZE, SHIFT,
     1      'R', 'Image')
         CALL FILGETMO ('Model', MODFILE)
         IF (ERROR) GO TO 999
         CALL MSGPUT ('Listing of model', 'I')
         CALL MODLIST ('Model')
         CALL MODIMG ('Model', 'Image')
         CALL MSGPUT ('Coordinates of model image:', 'I')
         CALL CRDLIST ('Image')
      END IF
C
      IF (PHOTONS.GT.0.0) THEN
         CALL MSGPUT ('Scaling model to unit peak flux', 'I')
         CALL ARRSTAT ('Image', ' ')
         CALL ARRSCALE ('Image', 1.0 / DATFGETR('Image', 'ARRMAX'),
     $      0.0, 'Image')
      END IF
C
      CALL FFTCONJA ('Image', 'XFR', DIR, 0)
      CALL FFTCONJA ('XFR', 'TPSF', DIR, 0)
      IF (ERROR) GO TO 999
      CALL MSGPUT ('Coordinates for PSF', 'I')
      CALL CRDLIST ('TPSF')
      CALL MSGPUT ('Coordinates for XFR', 'I')
      CALL CRDLIST ('XFR')
      CALL DATPUTC ('TPSF', 'TELESCOP', 'AIRY', 1)
      CALL DATPUTR ('TPSF', 'TELDIAM', TELDIAM, 1)
      CALL IMGCLONE ('TPSF', 'PSF')
C
C Make theoretical PSF
C
      CALL MSGPUT ('Making theoretical PSF', 'I')
      CALL DATPUTC ('Point', 'TELESCOP', 'AIRY', 1)
      IF(DFT) THEN
         CALL VISDFTPB ('Point', 'MOD/I', 'TPSF')
      ELSE
         CALL VISTOIMG ('Point', 'OBS/I', 'TPSF', 'XFR', .FALSE.)
         CALL IMGPB ('TPSF', 'TPSF', 'APPLY')
      END IF
      CALL ARRSTAT ('TPSF', ' ')
      IF (ERROR) GO TO 999
      SCALE = 1.0 / DATFGETR ('TPSF', 'ARRSUM')
      CALL ARRSCALE ('TPSF', SCALE, 0.0, 'TPSF')
      IF (TPSFFILE.NE.' ') THEN
         CALL FILIMGPU ('TPSF', TPSFFILE, ' ')
      END IF
      CALL IMGMAKEX ('TPSF', 'TXFR')
      CALL IMGLDEC ('TPSF', 'TXFR', MIND, 'PSF', 'MVis')
      CALL DATDELET ('TPSF')
      IF (BEAM(1) .EQ. 0. ) THEN
         CALL IMGBMSHP('PSF')
         IF (ERROR) GO TO 999
         BEAM(1) =  DATFGETR('PSF','BMAJ')*3600.0
         BEAM(2) =  DATFGETR('PSF','BMIN')*3600.0
         BEAM(3) =  DATFGETR('PSF','BPA')
      ENDIF
      IF (BEAM(1).GT.0.0) THEN
         CALL MSGPUT ('Smoothing beam', 'I')
         CALL IMGSMOOT ('PSF', BEAM, 'PSF', 'MVis')
      END IF
      IF (PSFFILE.NE.' ') THEN
         CALL FILIMGPU ('PSF', PSFFILE, ' ')
      END IF
C
C Make background image
C
      CALL IMGCLONE ('Image', 'Background')
      CALL ARRSETCO ('Background', 0.0, 1.0)
C
C Make the real PSF and convolve the model with it
C
      SEED = 2000001
      IF (DRTFILE.NE.' ') THEN
         IF (ERROR) GO TO 999
         CALL MSGPUT ('Making dirty image', 'I')
         CALL IMGCLONE ('MVis', 'GVis')
         CALL IMGCLONE ('PSF', 'Dirty')
         CALL IMGCLONE ('PSF', 'Snap')
         CALL MSGPUT ('Making dirty image as sequence of snapshots',
     $      'I')
         CALL ARRCOPY ('Point/OBS/I/WT', 'Point/OBS/I/OLDWT')
         IF(PHOTONS.GT.0.0) THEN
            CALL MSGPUT ('Adding photon noise', 'I')
            WRITE (MESSAGE, 1000) PHOTONS
 1000       FORMAT ('Scaling integrated flux to ',1Pe12.3,
     $         ' photons/second')
            CALL MSGPUT (MESSAGE, 'I')
            CALL ARRLC('Image', PHOTONS * TINT, 'Background',
     $         BACK  * TINT, 'Image')
         END IF
         CALL IMGFFT ('Image', 'MVis')
         DO 10 INTNDX = 1, NUMINT
            TIME(1) = (FLOAT(INTNDX-1)-0.5)*TINT
            TIME(2) = (FLOAT(INTNDX-1)+0.5)*TINT
            CALL VISSEL ('Point', 'OBS/I', TIME, UVLIMITS, NSEL)
            WRITE (MESSAGE, 1250) INTNDX, NSEL
 1250       FORMAT ('Integration ',I4,' selected ',I6,' points')
            CALL MSGPUT (MESSAGE, 'I')
            IF(DFT) THEN
               CALL VISDFTPB ('Point', 'OBS/I', 'Snap')
            ELSE
               CALL VISTOIMG ('Point', 'OBS/I', 'Snap', 'GVis', .FALSE.)
               CALL IMGPB ('Snap', 'Snap', 'APPLY')
            END IF
            CALL ARRSCALE ('Snap', SCALE, 0.0, 'Snap')
            CALL IMGFFT ('Snap', 'XFR')
            CALL ARRMULT ('MVis', 'XFR', 'GVis')
            CALL IMGFFT ('GVis', 'Snap')
            IF (PHOTONS.GT.0.0) THEN
               CALL ARRPOISS('Snap', 'Snap', SEED)
            END IF
            IF (INTNDX.EQ.1) THEN
               CALL ARRCOPY ('Snap', 'Dirty')
            ELSE
               CALL ARRADD ('Snap', 'Dirty', 'Dirty')
            END IF
            CALL ARRCOPY ('Point/OBS/I/OLDWT', 'Point/OBS/I/WT')
            IF (ERROR) GO TO 999
 10      CONTINUE
         CALL IMGLDEC ('Dirty', 'TXFR', MIND, 'Dirty', 'MVis')
         IF (BEAM(1).GT.0.0) THEN
            CALL MSGPUT ('Smoothing inverse-filtered dirty image', 'I')
            CALL IMGSMOOT ('Dirty', BEAM, 'Dirty', 'MVis')
         END IF
         CALL ARRSCALE ('Dirty', 1.0/FLOAT(NUMINT), 0.0, 'Dirty')
         CALL DATPUTC ('Dirty', 'BUNIT', 'PH/PIX/S', 1)
         CALL FILIMGPU ('Dirty', DRTFILE, ' ')
      END IF
C
 999  CONTINUE
      END
      SUBROUTINE SIUV (NAME)
C
C Make a uv data base for SI Solar interferometer
C
C
C	NAME	CH*(*)	input	Name of directory entry
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'SIUV')
C
      REAL		R, T, THETA, D2R
      INTEGER		NVIS, I, IA1, IA2, INTNDX
      INTEGER		UADD, VADD, WADD, TADD, BADD, VSADD, WTADD
      CHARACTER*(SYSMXNAM)	STRM2
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     1			CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
C
#include	"si.h"
C
      DATA		NAXIS	/SYSMXDIM*1/
      DATA		CTYPE	/SYSMXDIM*' '/
      DATA		CRPIX	/SYSMXDIM*0.0/
      DATA		CDELT	/SYSMXDIM*0.0/
      DATA		CROTA	/SYSMXDIM*0.0/
      DATA		CRVAL	/SYSMXDIM*0.0D0/
C=======================================================================
      D2R = ATAN(1.0)/45.0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (THINT.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero angle increment')
         GO TO 999
      END IF
C
      CALL DATCREAT (NAME)
      CALL DATCREAT (STRM2(NAME, 'OBS'))
      CALL DATCREAT (STRM2(NAME, 'OBS/I'))
C
      CALL DATPUTC (NAME, 'OBJECT', 'SUN', 1)
      CALL DATPUTC (NAME, 'TELESCOP', 'AIRY', 1)
      CALL DATPUTR (NAME, 'TELDIAM', TELDIAM, 1)
      CALL DATPUTC (NAME, 'INSTRUME', 'SI', 1)
      CALL DATPUTC (NAME, 'OBSERVER', 'CORNWELL', 1)
      CALL DATPUTC (NAME, 'DATE-OBS', '01/01/01', 1)
C
      NAX = 3
      CRVAL(1) = 0.0
      CRVAL(2) = 0.0
      CRVAL(3) = FREQ
      CTYPE(1) = 'RA'
      CTYPE(2) = 'DEC'
      CTYPE(3) = 'FREQ'
      CDELT(3) = BAND
      CALL CRDPUT (STRM2(NAME, 'OBS/I'), NAX, CTYPE, NAXIS, CRVAL, 
     1   CRPIX, CDELT, CROTA)
C
C Now make the various arrays
C
      NVIS = NBASE * NUMINT
      NAX = 1
      NAXIS(1) = NVIS
C
      CALL DATMAKAR (STRM2(NAME, 'UU'), NAX, NAXIS, 'R', UADD)
      CALL DATMAKAR (STRM2(NAME, 'VV'), NAX, NAXIS, 'R', VADD)
      CALL DATMAKAR (STRM2(NAME, 'WW'), NAX, NAXIS, 'R', WADD)
      CALL DATMAKAR (STRM2(NAME, 'BASELINE'), NAX, NAXIS, 'R', BADD)
      CALL DATMAKAR (STRM2(NAME, 'TIME'), NAX, NAXIS, 'R', TADD)
      CALL DATMAKAR (STRM2(NAME, 'OBS/I/VIS'), NAX, NAXIS, 'X', VSADD)
      CALL DATMAKAR (STRM2(NAME, 'OBS/I/WT'), NAX, NAXIS, 'R', WTADD)
      IF (ERROR) GO TO 990
C
C Set up baseline array and u,v arrays
C
      T = 0.0
      THETA = 0.0
      DO 22 INTNDX = 1, NVIS, NBASE
         I = 0
         DO 21 IA1 = 1, NANT
            DO 20 IA2 = IA1, NANT
               MEMR (BADD + INTNDX + I - 1) = FLOAT (256 * IA1 + IA2)
               R = (ANTLOC(IA2) - ANTLOC(IA1)) / WAVE
               MEMR (UADD + INTNDX + I - 1) = R * COS(THETA)
               MEMR (VADD + INTNDX + I - 1) = R * SIN(THETA)
               MEMR (WADD + INTNDX + I - 1) = 0.0
               MEMR (WTADD + INTNDX + I - 1) = 1.0
               MEMR (TADD + INTNDX + I - 1) = T
               I = I + 1
  20        CONTINUE
  21     CONTINUE
         THETA = THETA + D2R * THINT
         T = T + TINT
  22  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
