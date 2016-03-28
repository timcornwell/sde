C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgunsmo.f	1.2	 8/4/94
C
      SUBROUTINE IMGUNSMO (IMG, BEAM, SIMG, XIMG)
C
CD Unsmooth an image with a specified Gaussian.
C
C	IMG	CH*(*)	input	Name of image to be smoothed, etc.
C	IMG/MINTAPER	R input	Minimum taper that will be removed
C	BEAM	REAL(*)	input	Major axis of convolving function,
C				measured in units of asec
C	SIMG	CH*(*)	input	Name of unsmoothed image
C	XIMG	CH*(*)	input	Name of transformed unsmoothed image
C
C Audit trail:
C	Original version: Cloned from imgsmoot.f
C				D.S.Briggs	16 July 1992
C	Added MINTAPER, enable EXACT FFT.  This is still a bit of kludge.
C	What we really need is to be able to pass in an XFR mask.
C				D.S.Briggs	Aug 4 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, SIMG, XIMG
      REAL		BEAM(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGUNSMO')
C
      INTEGER		BCEN(SYSMXDIM)
      CHARACTER*1	ATYPE
      INTEGER		IAX, NAX, ADD, NAXIS(SYSMXDIM), NDUMMY
      REAL		TAPER(4), FACT, DELT(SYSMXDIM), MINTAPER
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST
      REAL		DATFGETR
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF (DATEXIST(STRM2(IMG,'MINTAPER'))) THEN
         MINTAPER = DATFGETR (IMG, 'MINTAPER')
      ELSE
         MINTAPER = 1.E-5
      END IF
C
C Transform input image
C
      CALL DATPUTC (IMG, 'FFTSIZE', 'EXACT', 1)
      CALL IMGFFT (IMG, XIMG)
C
C Set center of tapering to middle of u,v plane
C
      CALL DATGETAR (XIMG, NAX, NAXIS, ATYPE, ADD)
      IF (ERROR) GO TO 990
      IF (ATYPE.NE.'X') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//ATYPE//' for Image')
         GO TO 990
      END IF
C
      BCEN(1) = 1
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
      TAPER(1) = FACT*FLOAT(NAXIS(2))/(BEAM(1)/ABS(3600*DELT(1)))
      TAPER(2) = FACT*FLOAT(NAXIS(2))/(BEAM(2)/ABS(3600*DELT(1)))
      TAPER(3) = 90.0 + BEAM(3)
      IF (NAXIS(3).GT.1) THEN
         TAPER(4) = FACT*FLOAT(NAXIS(3))/(ABS(BEAM(4)/(3600*DELT(3))))
      END IF
C
C Another work array
C
      CALL IMGCLONE (XIMG, 'UnsmoTaper')
      CALL ARRSETCO ('UnsmoTaper', 0.0, 1.0)
C
C Now perform taper
C
      CALL ARRGTAPE ('UnsmoTaper', TAPER, BCEN, 'UnsmoTaper')
      CALL ARRCVTR ('UnsmoTaper','UnsmoTaper')
      CALL ARRCLIP ('UnsmoTaper', MINTAPER, 1.E20, 'UnsmoTaper')
      CALL ARRCVTX ('UnsmoTaper', 'UnsmoTaper')
      CALL ARRDIV (XIMG, 'UnsmoTaper', XIMG)
C
C Inverse transform to get result
C
      CALL IMGCLONE (XIMG, 'UnsmoTempImg')
      CALL ARRCOPY (XIMG, 'UnsmoTempImg')
      CALL DATPUTC ('UnsmoTempImg', 'FFTSIZE', 'EXACT', 1)
      CALL IMGFFT ('UnsmoTempImg', SIMG)
C
C Clean up
C
      CALL DATDELET ('UnsmoTaper')
      CALL DATDELET ('UnsmoTempImg')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
