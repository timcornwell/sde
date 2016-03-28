C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)psfdiff.f	1.2    1/4/95
C
      SUBROUTINE SDEMAIN
C
CD Examine differences in fitted and actual PSFs
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Feb 28 1994
C	Track changes to FILBEMGE
C				D.S.Briggs	Dec 30 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PSFDIFF')
C
      INTEGER		NDUMMY
      CHARACTER*(SYSMXNAM)	PSFFILE, BEAMFILE, OUTFILE, OUTBFILE,
     $   		FITALG
      REAL		RMAX, BEAM(4), F,
     $   		RPIX(SYSMXDIM), DELT(SYSMXDIM)
      INTEGER		NPOINTS, NAX, NAXIS(SYSMXDIM), PADD, MADD
      CHARACTER*1	ATYPE
      LOGICAL		DOCURVE
C
      INTEGER		STRLEN, DATADD
C==================================================================
      CALL MSGWELCO ('I take take differences of fitted/actual PSFs')
      CALL USRCTL
C
C Get input image
C
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Text', OUTFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('BeamFile', BEAMFILE, 1, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETC ('OutBeam', OUTBFILE, 1, NDUMMY)
      CALL USRGETR ('Rmax', RMAX, 1, NDUMMY)
      CALL USRGETI ('NPoints', NPOINTS, 1, NDUMMY)
C
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
      CALL FILBEMGE (BEAMFILE, BEAM, FITALG, 'BEAM')
      CALL DATPUTC ('PSF', 'FFTSIZE', 'EXACT', 1)
      CALL DATGETAR ('PSF', NAX, NAXIS, ATYPE, PADD)
C
      CALL IMGCLONE ('PSF','ModelPSF')
      CALL ARRSETCO ('ModelPSF', 0.0, 0.0)
      CALL DATGETR ('ModelPSF', 'CRPIX', RPIX, SYSMXDIM, NDUMMY)
      CALL DATGETR ('ModelPSF', 'CDELT', DELT, SYSMXDIM, NDUMMY)
      F = ATAN(1.0)/LOG(2.0) * BEAM(1) * BEAM(2) /
     $   ABS(DELT(1) * DELT(2) * 3600.0**2)
      MADD = DATADD ('ModelPSF')
      CALL MODIMG2D (1, F, 0.0, 0.0, BEAM(1), BEAM(2), BEAM(3),
     $   'GAUSS', MEMR(MADD), NAXIS(1), NAXIS(2), RPIX(1),
     $   DELT(1)*3600.0, RPIX(2), DELT(2)*3600.0)
      IF (ERROR) GO TO 999
C
      IF (OUTBFILE.NE.' ') CALL FILIMGPU ('ModelPSF', OUTBFILE, ' ')
      IF (ERROR) GO TO 999
C
      IF (RMAX.LT.0) RMAX = 2.0 * ABS(RMAX) / SQRT(BEAM(1)*BEAM(2))
C
      DOCURVE = (OUTFILE.NE.' ')
      IF (DOCURVE) THEN
         MESSAGE = 'Opening ' // OUTFILE(1:STRLEN(OUTFILE)) //
     $      ' for WRITE as Output'
         CALL MSGPUT (MESSAGE, 'I')
         CALL FILDEL (OUTFILE)
         CALL TXTOPEN ('Output', OUTFILE, 'WRITE')
      END IF
C
      CALL PIXRPSDF (MEMR(PADD), MEMR(MADD), NAXIS(1), NAXIS(2),
     $   RMAX, NPOINTS, 'Output', DOCURVE)
C
      IF (OUTFILE.NE.' ') CALL TXTCLOSE ('Output')
C
 999  CONTINUE
      END


