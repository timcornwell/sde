C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrrdclp.f	1.1	 3/15/91
C
      SUBROUTINE ARRRDCLP (IMAGE, MINRAD, MAXRAD, CLIP)
C
CD Clips IMAGE to CLIP for R < MINRAD and R > MAXRAD
C
C
C	IMAGE	CH*(*)	in/out	IMAGE to be clipped
C	MINRAD	REAL	input	min radius (same units as CDELT)
C	MAXRAD	REAL	input	max radius (same units as CDELT)
C	CLIP	REAL	input	value to which image is clipped
C
C Audit trail:
C				M.A.Holdaway	March 12 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IMAGE
      REAL		CLIP, MAXRAD, MINRAD
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRRDCLP')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), IADD, NDUMMY
      CHARACTER*1	T
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM), 
     $   		CROTA(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IMAGE, NAX, NAXIS, T, IADD)
      CALL CRDGET   (IMAGE, NAX, TYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $   CROTA)
C
      IF (NAXIS(2) .EQ. 1 .OR. NAXIS(3) .GT. 1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Can only do 2-D arrays')
         GOTO 990
      ENDIF
      IF (CRPIX(2) .EQ. 1) CRPIX(2) = NAXIS(2)/2
      IF (T .EQ. 'R') THEN
         CALL PIXRRDCL (MEMR(IADD), NAXIS(1), NAXIS(2), CRPIX(1), 
     $      CRPIX(2), CDELT(1), CDELT(2), MINRAD, MAXRAD, CLIP)
      ELSE IF (T .EQ. 'X') THEN
         CALL PIXXRDCL (MEMX(IADD), NAXIS(1), NAXIS(2), CRPIX(1), 
     $      CRPIX(2), CDELT(1), CDELT(2), MINRAD, MAXRAD, CLIP)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Cannot deal with image type: '//T)
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END


