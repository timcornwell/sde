C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgmakex.f	1.4    2/18/92
C
      SUBROUTINE IMGMAKEX (PSF, XFR)
C
CD Make transfer function corresponding to a given PSF: we need to
C first transform. Next we must shift the reference pixel to the
C conventional spot: NX/2, NY/2, NZ/2, etc. 
C
C	PSF	CH*(*)	input	Name of PSF
C	XFR	CH*(*)	input	Name of XFR
C Audit trail:
C	Copy history info
C				T.J.Cornwell	Jan 10 1989
C	Removed spurious conjugation
C				T.J.Cornwell	July 21 1990
C	Fix calculation of reference pixel in odd image case.
C				D.S.Briggs	Jan 15 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	PSF, XFR
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGMAKEX')
C
      INTEGER		NDUMMY, IAX, NAX, NAXIS(SYSMXDIM)
      CHARACTER*1	TYPE
      REAL		RPIX(SYSMXDIM), SHIFT(SYSMXDIM),
     1			NRPIX (SYSMXDIM)
      DATA		SHIFT /SYSMXDIM*0.0/
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL IMGFFT (PSF, XFR)
C
C Find required phase rotation to put reference pixel at standard
C place
C
      CALL DATGETR (PSF, 'CRPIX', RPIX, SYSMXDIM, NAX)
      CALL DATGETAR (PSF, NAX, NAXIS, TYPE, NDUMMY)
      DO 10 IAX = 1, NAX
         IF(NAXIS(IAX).GT.1) THEN
            NRPIX (IAX) = (NAXIS(IAX)+1)/2
            SHIFT (IAX) = (NRPIX(IAX) - RPIX(IAX))/NAXIS(IAX)
         ELSE
            NRPIX (IAX) = 1.0
            SHIFT (IAX) = 0.0
         END IF
  10  CONTINUE
      CALL ARRPHASE (XFR, SHIFT)
C
C Fix reference pixel of header
C
      CALL DATGETR (XFR, 'CRPIX', RPIX, SYSMXDIM, NAX)
      DO 20 IAX = 1, NAX
         RPIX(IAX) = 1.0
  20  CONTINUE
      CALL DATPUTR (XFR, 'CRPIX', RPIX, NAX)
C
C Copy history info
C
      CALL HISCOPY (PSF, XFR)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
