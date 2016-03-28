C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hstsim.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to simulate observations for the HST
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HSTSIM')
C
      CHARACTER*(SYSMXNAM) IMGFILE, DRTFILE, PSFFILE
      INTEGER		NDUMMY, SEED
      REAL		DATFGETR
      REAL		PHOTONS
      REAL		READOUT, SCALE, A, B
C
#include		"hst.h"
C
C==================================================================
      CALL MSGWELCO ('I simulate HST data (with aberrations)')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETR ('Photons', PHOTONS, 1, NDUMMY)
      CALL USRGETR ('Readout', READOUT, 1, NDUMMY)
      CALL USRGETR ('Scale', SCALE, 1, NDUMMY)
      CALL USRGETC ('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETR ('A', A, 1, NDUMMY)
      CALL USRGETR ('B', B, 1, NDUMMY)
      CALL USRGETR ('Wave', WAVE, 1, NDUMMY)
      FREQ = 3.0E8 / WAVE
C
      WRITE (MESSAGE,1240) WAVE*1E6
 1240 FORMAT ('Wavelength = ',F8.3,' microns')
      CALL MSGPUT (MESSAGE, 'I')
C
C Now get model
C
      CALL MSGPUT ('Getting model image', 'I')
      IF (IMGFILE.NE.' ') THEN
         CALL FILIMGGE ('Image', IMGFILE, ' ')
         CALL MSGPUT ('Coordinates of model image:', 'I')
         CALL CRDLIST ('Image')
      END IF
C
      IF (PHOTONS.GT.0.0) THEN
         CALL MSGPUT ('Scaling model to unit peak flux', 'I')
      END IF
C
      CALL IMGFFT ('Image', 'IllumC')
      CALL IMGCLONE ('Image', 'Efield1')
      CALL IMGCLONE ('Efield1', 'Efield2')
      CALL IMGCLONE ('IllumC', 'XFR')
      CALL IMGCLONE ('IllumC', 'XFRC')
      CALL IMGCLONE ('IllumC', 'XFRS')
      CALL IMGCLONE ('IllumC', 'IllumS')
C
      CALL MSGPUT ('Making HST Illumination pattern', 'I')
      CALL MSGPUT ('Coordinates for illumination pattern', 'I')
      CALL CRDLIST ('IllumC')
      CALL HSTILL ('IllumC', 'IllumS', A, B)
C
      CALL MSGPUT ('Making transfer function', 'I')
      CALL IMGFFT ('IllumC', 'Efield1')
      CALL ARRMULT ('Efield1', 'Efield1', 'Efield1')
      CALL IMGFFT ('Efield1', 'XFRC')
      CALL IMGFFT ('IllumS', 'Efield1')
      CALL ARRMULT ('Efield1', 'Efield1', 'Efield1')
      CALL IMGFFT ('Efield1', 'XFRS')
      CALL ARRADD ('XFRC', 'XFRS', 'XFR')
C
C Make point spread functiion
C
      CALL IMGFFT ('XFR', 'PSF')
      CALL ARRSTAT ('PSF', ' ')
      CALL ARRSCALE ('PSF', 1.0/DATFGETR('PSF','ARRSUM'), 0.0, 'PSF')
      CALL DATDELET ('XFR')
C
      IF (PSFFILE.NE.' ') THEN
         CALL DATPUTC ('PSF', 'TELESCOP', 'HST', 1)
         CALL FILIMGPU ('PSF', PSFFILE, ' ')
      END IF
C
      IF (DRTFILE.NE.' ') THEN
         CALL IMGCLONE ('Image', 'Dirty')
         IF (ERROR) GO TO 999
         CALL MSGPUT ('Making dirty image', 'I')
C
C Scale for photon noise
C
         IF(PHOTONS.GT.0.0) THEN
            SEED = 2000001
            CALL IMGCLONE ('Image', 'Background')
            CALL MSGPUT ('Adding photon noise', 'I')
            WRITE (MESSAGE, 1000) PHOTONS
 1000       FORMAT ('Scaling integrated flux to ',1Pe12.3,
     $         ' photons')
            CALL MSGPUT (MESSAGE, 'I')
            CALL ARRSTAT ('Image', ' ')
            CALL ARRSCALE('Image',PHOTONS / DATFGETR('Image', 'ARRMAX'),
     $         0.0, 'Image')
         END IF
C
C Convolve model with PSF
C
         CALL IMGCONV ('Image', 'PSF', 'XFR', 'Dirty', 'MVis')
C
C Now actually add photon noise
C
         IF (PHOTONS.GT.0.0) THEN
            CALL ARRPOISS('Dirty', 'Dirty', SEED)
         END IF
C
C Convert to data numbers and add readout noise
C
         CALL DATPUTC ('Dirty', 'BUNIT', 'DN', 1)
         CALL ARRSCALE ('Dirty', 1.0/SCALE, 0.0, 'Dirty')
         CALL ARRGAUSS ('Dirty', 'Dirty', READOUT, 2000001)
         CALL FILIMGPU ('Dirty', DRTFILE, ' ')
      END IF
C
 999  CONTINUE
      END
C++
      SUBROUTINE HSTILL (NAMEC, NAMES, A, B)
C
C Make a illumination pattern for the HST
C
C	NAMEC	CH*(*)	input	Name of directory entry of cos part
C	NAMES	CH*(*)	input	Name of directory entry of sin part
C	A, B	REAL	input	Aberration constants (pp 488 B&W)
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
      CHARACTER*(*)	NAMEC, NAMES
      REAL		A, B
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'HSTILL')
C
      INTEGER		IMCADD, IMSADD
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     1			CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
C
      INTEGER		IU, IV, INDX
      REAL		R, RHO, RHOMIN, D2R, PHASE, INNERR, OUTERR
      CHARACTER*1	ATYPE
C
#include	"hst.h"
C
C=======================================================================
      D2R = ATAN(1.0)/45.0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      OUTERR = 1.2
      INNERR = 0.45
C
      RHOMIN = INNERR / OUTERR
C
      CALL DATPUTC (NAMEC, 'OBJECT', 'FAKE', 1)
      CALL DATPUTC (NAMEC, 'TELESCOP', 'HST', 1)
      CALL DATPUTC (NAMEC, 'INSTRUME', 'HST', 1)
      CALL DATPUTC (NAMEC, 'OBSERVER', 'CORNWELL', 1)
      CALL DATPUTC (NAMEC, 'DATE-OBS', '01/01/01', 1)
C
      CALL DATGETAR (NAMEC, NAX, NAXIS, ATYPE, IMCADD)
      CALL DATGETAR (NAMES, NAX, NAXIS, ATYPE, IMSADD)
      CALL CRDGET (NAMEC, NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT, CROTA)
C
C Now fill in the illumination pattern
C
      DO 10 IV = 1, NAXIS(2)
         DO 11 IU = 1, NAXIS(1)
            R = WAVE * SQRT((CDELT(1)*(FLOAT(IU)-CRPIX(1)))**2 +
     $                      (CDELT(2)*(FLOAT(IV)-CRPIX(2)))**2)
            RHO = R / OUTERR
            INDX = NAXIS(1) * (IV - 1) + (IU - 1)
            IF((RHO.LE.1.0).AND.(RHO.GT.RHOMIN)) THEN
               PHASE = D2R * 360.0 * A * (RHO**4 + B * RHO**2)
               IF(PHASE.NE.0.0) THEN
                  MEMX(IMCADD+INDX) = COS(PHASE)
                  MEMX(IMSADD+INDX) = SIN(PHASE)
               ELSE
                  MEMX(IMCADD+INDX) = 1.0
                  MEMX(IMSADD+INDX) = 0.0
               END IF
            ELSE
               MEMX(IMCADD+INDX) = 0.0
               MEMX(IMSADD+INDX) = 0.0
            END IF
 11      CONTINUE
 10   CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
