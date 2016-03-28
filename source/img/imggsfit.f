C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imggsfit.f	1.2    6/17/94
C
      SUBROUTINE IMGGSFIT (IMG)
C
CD Fits a single Gaussian to the image and writes the fitted parameters
C to the image header as GFLUX, GAMP, GMAJ, GMIN, GPA, GDX, GDY, GZ
C
C GAMP is in the flux units of the image
C GMAJ, GMIN is in arcseconds
C GPA is position angle in degrees, anti-clockwise from vertical
C GDX, GDY is position offset from the reference position in arcsec
C GZ is in principle the zero offset, also in flux units, but is
C currently forced to zero.
C
C	IMG	CH*(*)	input	Name of point spread function
C
C Audit trail:
C	Original version
C				D.S.Briggs	March 20 1994
C	Turn off MSG system during initial linear seed.
C				D.S.Briggs	June 14 1994
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGGSFIT')
C
      CHARACTER*(*)	IMG
C
      CHARACTER*1	T
      INTEGER		N, P, NDUMMY, ADD, NAXIS(SYSMXDIM)
      INTEGER		NROW(SYSMXDIM), IPEAK(SYSMXDIM), MAXPTS, SADD
      REAL		AMIN, BMAJA, BMINA, CRPIX(SYSMXDIM)
      REAL		CDELT(SYSMXDIM), BMAJ, BMIN, BPA, BAMP,
     $   		BDX, BDY, BZ, FLUX
C
      REAL		DATFGETR
      INTEGER		PIXISAMA
C=======================================================================
      IF (ERROR) GO TO 999
C
C Initialize the search with the beam fit algorithm.
C
      SYSMSG = .FALSE.
      CALL DATPUTC (IMG, 'FIT-ALG', 'NONLINEAR', 1)
      CALL IMGBMSHP (IMG)
      SYSMSG = .TRUE.
C
      BMAJ = DATFGETR (IMG, 'BMAJ')
      BMIN = DATFGETR (IMG, 'BMIN')
      BPA = DATFGETR (IMG, 'BPA')
      BZ = DATFGETR (IMG, 'BZ')
C
      AMIN = .35
      BAMP = 0.
      CALL DATGETAR(IMG, N, NAXIS, T, ADD)
      CALL DATGETR(IMG, 'CDELT', CDELT, N, NDUMMY)
      CALL DATGETR(IMG, 'CRPIX', CRPIX, N, NDUMMY)
      IF (ERROR) GOTO 990
      IF(T .NE. 'R') THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE, 
     $      'IMG image type ='//T)
         GOTO 990
      ENDIF
      IF (NAXIS(3).GT.1) THEN
         CALL ERRREPOR(ERRFATAL, ROUTINE, 'Only programmed for 2-D')
         GO TO 990
      END IF
C
      P = PIXISAMA(NAXIS(1)*NAXIS(2), MEMR(ADD), 1) - 1
      IPEAK(1) = 1 + MOD(P, NAXIS(1))
      IPEAK(2) = 1 + (P - IPEAK(1) + 1)/NAXIS(1)
      IF (IPEAK(1).GT.NAXIS(1)) IPEAK(1) = NAXIS(1) / 2
      IF (IPEAK(2).GT.NAXIS(2)) IPEAK(2) = NAXIS(2) / 2
C						Gaussian to be fit to
C						2*NROW(1)+1 by 2*NROW(2)+1
C						patch of IMG about IPEAK
      NROW(1) = 100
      NROW(2) = 100
      MAXPTS = (2*NROW(1)+1)*(2*NROW(2)+1)
C
      CALL DATMAKAR ('BmshpScratch', 1, 4*MAXPTS, 'R', SADD)
      CALL PIXGSFIT (MEMR(ADD), NAXIS(1), NAXIS(2), CDELT, IPEAK,
     $   NROW, AMIN, MEMR(SADD), MEMR(SADD+MAXPTS),
     $   MEMR(SADD+2*MAXPTS), MEMR(SADD+3*MAXPTS),
     $   BMAJ, BMIN, BPA, BZ, BAMP, BDX, BDY)
      CALL DATDELET ('BmshpScratch')
      IF (ERROR) GO TO 990
C
      BMAJA = BMAJ*3600.
      BMINA = BMIN*3600.
      BDX = (IPEAK(1)+BDX-CRPIX(1))*CDELT(1)*3600.
      BDY = (IPEAK(2)+BDY-CRPIX(2))*CDELT(2)*3600.
      FLUX = ATAN(1.0)/LOG(2.0)*BAMP*(BMAJ*BMIN)/ABS(CDELT(1)*CDELT(2))
C
      CALL DATPUTR(IMG, 'GFLUX', FLUX, 1)
      CALL DATPUTR(IMG, 'GAMP', BAMP, 1)
      CALL DATPUTR(IMG, 'GMAJ', BMAJA, 1)
      CALL DATPUTR(IMG, 'GMIN', BMINA, 1)
      CALL DATPUTR(IMG, 'GPA', BPA, 1)
      CALL DATPUTR(IMG, 'GDX', BDX, 1)
      CALL DATPUTR(IMG, 'GDY', BDY, 1)
      CALL DATPUTR(IMG, 'GZ', BZ, 1)
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
