C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsmoot.f	1.6    6/7/93
C
      SUBROUTINE IMGSMOOT (IMG, BEAM, SIMG, SWORK)
C
CD Smooth an image with a specified Gaussian.
C
C	IMG	CH*(*)	input	Name of image to be smoothed, etc.
C	BEAM	REAL(*)	input	Major axis of convolving function,
C				measured in units of asec or klambda
C	SIMG	CH*(*)	input	Name of smoothed image
C	SWORK	CH*(*)	input	Name of Scratch array for transform
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed to use asec
C				T.J. Cornwell	Nov 22 1989
C	Updated ref pix calcs to allow for odd axis size
C				D.S.Briggs	May 6 1992
C	1-D smoothing added
C				D.S.Briggs	Oct 21 1992
C	Added complex smoothing ability.  Measure FWHM in klambda
C				D.S.Briggs	24 Jan 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, SIMG, SWORK
      REAL		BEAM(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSMOOT')
C
      INTEGER		BCEN(SYSMXDIM)
      CHARACTER*1	ATYPE
      INTEGER		IAX, FNU, NAX, ADD, NAXIS(SYSMXDIM), NDUMMY
      REAL		TAPER(4), FACT, DELT(SYSMXDIM)
C
c      CHARACTER*(SYSMXNAM)	STRM2
c      LOGICAL		DATEXIST
      INTEGER		DATFGETI
C=====================================================================
      IF (ERROR) GO TO 999
C
C Transform input image
C
      CALL IMGFFT (IMG, SWORK)
C
      CALL DATGETAR (SWORK, NAX, NAXIS, ATYPE, ADD)
      IF (ERROR) GO TO 990
      IF ((ATYPE.NE.'X').AND.(ATYPE.NE.'R')) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//ATYPE//' for Image')
         GO TO 990
      END IF
C
C Set center of tapering appropriately
C
      IF (ATYPE.EQ.'X') THEN
         FNU = DATFGETI (SWORK,'FNU')
         BCEN(1) = 1
      ELSE
         FNU = NAXIS(1)
         BCEN(1) = (FNU+1) / 2
      END IF
C
      DO 10 IAX = 2, NAX
         IF (NAXIS(IAX).GT.1) THEN
            BCEN(IAX) = (NAXIS(IAX)+1) / 2
         ELSE
            BCEN(IAX) = 1
         END IF
  10  CONTINUE
C
C Convert specified beam to taper: Warning: this is not general
C enough! Fix soon!
C
      CALL DATGETR (IMG, 'CDELT', DELT, SYSMXDIM, NDUMMY)
      FACT = LOG(2.0)/ATAN(1.0)
C
      IF (ATYPE.EQ.'X') THEN
         TAPER(1) = FACT*FLOAT(FNU)/(BEAM(1)/ABS(3600*DELT(1)))
         TAPER(2) = FACT*FLOAT(NAXIS(2))/(BEAM(2)/ABS(3600*DELT(1)))
         TAPER(3) = 90.0 + BEAM(3)
         IF (NAXIS(3).GT.1) THEN
            TAPER(4) = FACT*FLOAT(NAXIS(3))/
     $           (ABS(BEAM(4)/(3600*DELT(3))))
         END IF
      ELSE
         TAPER(1) = FACT*FLOAT(FNU)/(BEAM(1)*1.E3/ABS(DELT(1)))
         TAPER(2) = FACT*FLOAT(NAXIS(2))/(BEAM(2)*1.E3/ABS(DELT(1)))
         TAPER(3) = 90.0 + BEAM(3)
         IF (NAXIS(3).GT.1) THEN
            TAPER(4) = FACT*FLOAT(NAXIS(3))/
     $           (ABS(BEAM(4)*1.E3/(DELT(3))))
         END IF
      END IF
C
C Now perform taper
C
      CALL ARRGTAPE (SWORK, TAPER, BCEN, SWORK)
C
C Inverse transform to get result
C
      CALL IMGFFT (SWORK, SIMG)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
