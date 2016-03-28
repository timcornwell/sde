C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpowf.f	1.4	 2/22/91
C
      SUBROUTINE IMGPOWF (IMG, INNER, OUTER, POWER, SIMG, SWORK)
C
CD Smooth an image with a power law function. The image transform
C is multiplied by:
C
C	1.0		if (R < INNER)
C (R/INNER)**POWER	if (OUTER > R > INNER)
C	0.0		if (R > OUTER)
C
C	IMG	CH*(*)	input	Name of image to be smoothed, etc.
C	INNER	REAL	input	Inner scale of function (wavelengths)
C	OUTER	REAL	input	Outer scale of function (wavelengths)
C	POWER	REAL	input	Power law index
C	SIMG	CH*(*)	input	Name of smoothed image
C	SWORK	CH*(*)	input	Name of Scratch array for transform
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Upgraded to deal with non-hermitian u-v planes
C				M.A.Holdaway	Feb 22 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, SIMG, SWORK
      REAL		INNER, OUTER, POWER
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPOWF')
C
      INTEGER		BCEN(SYSMXDIM)
      CHARACTER*1	ATYPE
      INTEGER		IAX, NAX, ADD, NAXIS(SYSMXDIM), NDUMMY
      REAL		SINNER, SOUTER, CDELT(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM)
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
      CALL DATGETR (SWORK, 'CRPIX', CRPIX, SYSMXDIM, NDUMMY)
      IF (CRPIX(1) .EQ. 1) THEN
         BCEN(1) = 1
      ELSE
         BCEN(1) = NAXIS(1) / 2
      ENDIF
C
      DO 10 IAX = 2, NAX
         IF (NAXIS(IAX).GT.1) THEN
            BCEN(IAX) = NAXIS(IAX) / 2
         ELSE
            BCEN(IAX) = 1
         END IF
  10  CONTINUE
C
      CALL DATGETR (SWORK, 'CDELT', CDELT, SYSMXDIM, NDUMMY)
      SINNER = INNER / ABS(CDELT(1))
      SOUTER = OUTER / ABS(CDELT(1))
C
C Now multiply by power law
C
      CALL ARRMPOW (SWORK, SINNER, SOUTER, POWER, BCEN, SWORK)
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
