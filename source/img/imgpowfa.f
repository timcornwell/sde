C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpowfa.f	1.2    5/29/91
C
      SUBROUTINE IMGPOWFA (IMG, INNER, MIDDLE, OUTER, POWER1, POWER2, 
     $   AVX, AVY, SIMG, SWORK)
C
CD Smooth an image with a kinked anisotropic power law function. 
C	The image transform is multiplied by:
C
C	1.0		if (R < INNER)
C (R/INNER)**POWER1	if (MIDDLE > R > INNER)
C A*(R/MIDDLE)**POWER2	if (OUTER > R > MIDDLE)
C	0.0		if ( R > OUTER  |  RX > OUTERX  |  RY > OUTERY )
C
C	IMG	CH*(*)	input	Name of image to be smoothed, etc.
C	INNER	REAL	input	Inner scale of function (wavelengths)
C	MIDDLE	REAL	input	Middle scale of function (wavelengths)
C	OUTER	REAL	input	Outer scale of function (wavelengths)
C	POWER1	REAL	input	Power law index from INNER to MIDDLE
C	POWER2	REAL	input	Power law index from MIDDLE to OUTER
C	AVX	REAL	input	Averaging distance X (image space units)
C	AVY	REAL	input	Averaging distance Y (image space units)
C	SIMG	CH*(*)	input	Name of smoothed image
C	SWORK	CH*(*)	input	Name of Scratch array for transform
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				Mark Holdaway	May 15 1991
C	Revamped pre-averaging to properly deal with the velocity
C				M.A.Holdaway	May 29 1991
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, SIMG, SWORK
      REAL		INNER, MIDDLE, OUTER, AVX, AVY,
     $   		POWER1, POWER2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPOWFA')
C
      INTEGER		BCEN(SYSMXDIM)
      CHARACTER*1	ATYPE
      INTEGER		IAX, NAX, ADD, NAXIS(SYSMXDIM), NDUMMY
      REAL		SINNER, SOUTER, SMIDDLE
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
      SINNER = INNER 
      SMIDDLE = MIDDLE 
      SOUTER = OUTER 
C
C Now multiply by power law
C
      CALL ARRMPOWA (SWORK, SINNER, SMIDDLE, SOUTER, POWER1, POWER2, 
     $   AVX, AVY, BCEN, SWORK)
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

