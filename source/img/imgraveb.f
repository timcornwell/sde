C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgraveb.f	1.4    03 Aug 1995
C
      SUBROUTINE IMGRAVEB (IMAGE, TBOX, RAVE, PIXELS, NBINS, SCALE,
     $   DOCOPY, UNDEF, AMODE)
C
CD Makes a radial average of the image in question
C
C	IMAGE	CH*(*)	input	Name of imgae to be aved
C	BOX	CH*(*)	input	Name of box image
C	RAVE	CH*(*)	out	Name of radial average array
C	PIXELS	CH*(*)	out/in	Name of pixel array
C				(what pixel number for the ith RAVE bin)
C	NBINS	INT	input	BINS in RAVE, PIXELS
C	SCALE	INT	input	Scale factor for pixel axis
C	DOCOPY	LOG	input	Copy undefined values from last good value?
C	UNDEF	REAL	input	Set to this value if DOCOPY=F and no samples
C	AMODE	CH*(*)	input
C
C Audit trail:
C	Added Box image, cloned from IMGRAVE
C				D.S.Briggs	Apr 15 1993
C	Added DOCOPY and UNDEF
C				D.S.Briggs	Oct 30 1994
C	Added averaging mode
C				D.S.Briggs	Dec 22 1994
C	Fixed the NAXIS dimension problem
C				M.A.Holdaway	Aug 3 1995
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IMAGE, RAVE, PIXELS, TBOX, AMODE
      REAL		UNDEF
      LOGICAL		DOCOPY
      INTEGER		NBINS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGRAVEB')
C
      INTEGER		NDUMMY, I
      INTEGER		IADD, NAXI, NAXISI(SYSMXDIM), NAXB,
     $   		NAXISB(SYSMXDIM)
      REAL		RPIXI(SYSMXDIM), DELTI(SYSMXDIM), SCALE, FREQ
      CHARACTER*(1)	TI, TB
      CHARACTER*(SYSMXNAM)	BOX
      INTEGER		RADD, PADD, BADD, NAXR, NAXISR(SYSMXDIM)
      LOGICAL		DATEXIST
      DATA		RPIXI/SYSMXDIM*0./
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (TBOX.EQ.' ') THEN
         BOX = 'IMGRAVEB-TMP'
         CALL ARRCOPY (IMAGE, BOX)
         CALL ARRSETCO (BOX, 0.0, 1.0)
      ELSE
         BOX = TBOX
      END IF
C
      CALL DATGETAR (IMAGE, NAXI, NAXISI, TI, IADD)
      CALL DATGETAR (BOX, NAXB, NAXISB, TB, BADD)
      CALL DATGETR  (IMAGE, 'CRPIX', RPIXI, SYSMXDIM, NDUMMY)
C
      IF (NAXISI(2) .EQ. 1 ) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Cannot work with 1-D image')
         GOTO 990
      ELSE IF ( NAXISI(3) .GT. 1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Cannot work with 3-D image')
         GO TO 990
      ELSE IF (TI .NE. 'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Cannot work with non-REAL image')
         GO TO 990
      ELSE IF (TB .NE. 'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Cannot work with non-REAL box image')
         GO TO 990
      ELSE IF ((NAXISI(1).NE.NAXISB(1)).OR.
     $      (NAXISI(2).NE.NAXISB(2))) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'image and box are different sizes')
         GO TO 990
      ELSE
C
C check to see that the output average array exists
C
         IF (.NOT. DATEXIST(RAVE)) THEN
            NAXR = 1
            NAXISR(1) = NBINS
            DO 20 I = 2, SYSMXDIM
               NAXISR(I) = 1
 20         CONTINUE
            CALL DATMAKAR (RAVE, NAXR, NAXISR, 'R', RADD)
         ENDIF
         IF (.NOT. DATEXIST(PIXELS)) THEN
            NAXR = 1
            NAXISR(1) = NBINS
            DO 30 I = 2, SYSMXDIM
               NAXISR(I) = 1
 30         CONTINUE
            CALL DATMAKAR (PIXELS, NAXR, NAXISR, 'R', PADD)
         ENDIF
C
C perform the radial average
C
         CALL ARRRAVEB (MEMR(IADD), MEMR(BADD), MEMR(RADD), MEMR(PADD),
     $        NAXISI(1), NAXISI(2), RPIXI,
     $        NBINS, SCALE, DOCOPY, UNDEF, AMODE)
      ENDIF
C
      IF (BOX.EQ.'IMGRAVEB-TMP') CALL DATDELET (BOX)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
