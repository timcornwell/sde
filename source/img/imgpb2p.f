C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpb2p.f	1.1	 6/5/93
C
      SUBROUTINE IMGPB2P (IMG, BEAM, SIMG)
C
CD Convert the units of an image to per beam
C
C
C	IMG	CH*(*)	input	Name of image to be scaled
C	BEAM	REAL(*)	input	Major axis of convolving function,
C				measured in asec
C	SIMG	CH*(*)	input	Name of scaled image
C Audit trail:
C	Original version: Cloned from IMGP2PB
C				D.S. Briggs	28 Oct 1992
C	Copy header when creating output
C				D.S. Briggs	18 Apr 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, SIMG
      REAL		BEAM(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPB2P')
C
      INTEGER		NDUMMY, NAX, NAXIS(SYSMXDIM), ADD
      CHARACTER*1	ATYPE
      REAL		FACT, DELT(SYSMXDIM), BFACT
C
      LOGICAL		DATEXIST
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
      CALL DATGETR (IMG, 'CDELT', DELT, SYSMXDIM, NDUMMY)
      CALL DATGETAR (IMG, NAX, NAXIS, ATYPE, ADD)
C
      FACT = SQRT(ATAN(1.0)/LOG(2.0))/3600.0
      IF (NAXIS(3).GT.1) THEN
         BFACT = FACT**3 * BEAM(1) * BEAM(2) * BEAM(4)
     $     / ABS(DELT(1)**2 * DELT(3))
      ELSE
         BFACT = FACT**2 * BEAM(1) * BEAM(2) / DELT(1)**2
      END IF
      IF (.NOT.DATEXIST(SIMG)) CALL IMGCLONE (IMG, SIMG)
      CALL ARRSCALE (IMG, 1.0/BFACT, 0.0, SIMG)
C
C Fill in header
C
      CALL DATPUTR (SIMG, 'BMAJ', BEAM(1)/3600.0, 1)
      CALL DATPUTR (SIMG, 'BMIN', BEAM(2)/3600.0, 1)
      CALL DATPUTR (SIMG, 'BPA',  BEAM(3), 1)
      CALL DATPUTR (SIMG, 'BZ', BEAM(4)/3600.0, 1)
      CALL DATPUTC (SIMG, 'BUNIT', 'JY', 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
