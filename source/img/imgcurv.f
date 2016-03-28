C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgcurv.f	1.2 7/15/92
C
      SUBROUTINE IMGCURV(IMG, SIMG, SWORK)
C
CD Form second derivative of an image
C
C
C	IMG	CH*(*)	input	Name of image to be , etc.
C	SIMG	CH*(*)	input	Name of smoothed image
C	SWORK	CH*(*)	input	Name of Scratch array for transform
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	July 15 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, SIMG, SWORK
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCURV')
C
      INTEGER		BCEN(SYSMXDIM), NREAL
      CHARACTER*1	ATYPE
      INTEGER		IAX, NAX, ADD, NAXIS(SYSMXDIM), NDUMMY
      REAL		DELT(SYSMXDIM)
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Transform input image
C
      CALL IMGFFT (IMG, SWORK)
C
C Set center of tapering to middle of u,v plane
C
      CALL DATGETAR (SWORK, NAX, NAXIS, ATYPE, ADD)
      IF (ERROR) GO TO 990
      IF (ATYPE.NE.'X') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//ATYPE//' for Image')
         GO TO 990
      END IF
C
      NREAL=1
      BCEN(1) = 1
      DO 10 IAX = 2, NAX
         IF (NAXIS(IAX).GT.1) NREAL = NREAL + 1
         IF (NAXIS(IAX).GT.1) THEN
            BCEN(IAX) = (NAXIS(IAX)+1) / 2
         ELSE
            BCEN(IAX) = 1
         END IF
         BCEN (IAX) = MAX (1, MIN (BCEN(IAX), NAXIS(IAX)))
  10  CONTINUE
C
      CALL DATGETR (SWORK, 'CDELT', DELT, SYSMXDIM, NDUMMY)
C
C Now actually call routine which does the work on the pixels. Branch
C here on data type of the array.
C
      IF ((ATYPE.EQ.'X').AND.(NREAL.EQ.2)) THEN
         CALL PIX2DCUR (MEMX(ADD), NAXIS(1), NAXIS(2),
     1      DELT(1), DELT(2), BCEN(1), BCEN(2), MEMX(ADD))
      ELSE 
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     &      'Cannot sensibly taper array ')
         GO TO 999
      END IF
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
