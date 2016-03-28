C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgrave.f	1.1	 3/15/91
C
      SUBROUTINE IMGRAVE (IMAGE, RAVE, PIXELS, NBINS, SCALE)
C
CD Makes a radial average of the image in question
C
C Arguments: CALL IMGRAVE (IMAGE, RAVE, PIXELS, NBINS, SCALE)
C
C	IMAGE	CH*(*)	input	Name of imgae to be aved
C	RAVE	CH*(*)	out	Name of radial average array
C	PIXELS	CH*(*)	out/in	Name of pixel array
C				(what pixel number for the ith RAVE bin)
C	NBINS	INT	in	BINS in RAVE, PIXELS
C	SCALE	INT	in	Scale factor for pixel axis
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Oct 11 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IMAGE, RAVE, PIXELS
      INTEGER		NBINS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGRAVE')
C
      INTEGER		NDUMMY, I
      INTEGER		IADD, NAXI, NAXISI(SYSMXDIM)
      REAL		RPIXI(SYSMXDIM), DELTI(SYSMXDIM), SCALE, FREQ
      CHARACTER*(1)	TI
      INTEGER		RADD, PADD, NAXR, NAXISR(SYSMXDIM)
      LOGICAL		DATEXIST
      DATA		RPIXI/SYSMXDIM*0./
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IMAGE, NAXI, NAXISI, TI, IADD)
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
         CALL ARRRAVE (MEMR(IADD), MEMR(RADD), MEMR(PADD), NAXISI, 
     $      RPIXI, NBINS, SCALE)
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
