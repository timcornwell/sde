C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgrmean.f	1.2 07 Nov 1995
C
      SUBROUTINE IMGRMEAN (IMAGE, RAVE, RDISP, PIXELS, NBINS, SCALE,
     $     SHAPE)
C
CD Makes a radial mean and rms about the mean of the image in question
C
C
C	IMAGE	CH*(*)	input	Name of imgae to be aved
C	RAVE	CH*(*)	in	Name of radial average array
C	RDISP	CH*(*)	in	Name of Dispersion about the mean
C	PIXELS	CH*(*)	in	Name of pixel array
C				(what pixel number for the ith RAVE bin)
C	NBINS	INT	in	BINS in RAVE, PIXELS
C	SCALE	INT	in	Scale factor for pixel axis
C	SHAPE	REAL(2)	in	S(1) = Elongation, S(2) = PA of Major Axis
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 14 1994
C	Changed call to ARRRMEAN to match revised inputs for that
C       routine.
C				M.P.Rupen       November 6 1995
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IMAGE, RAVE, RDISP, PIXELS
      INTEGER		NBINS
      REAL		SHAPE(2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGRMEAN')
C
      INTEGER		NDUMMY, I
      INTEGER		IADD, NAXI, NAXISI(SYSMXDIM)
      integer           NX
      REAL		RPIXI(SYSMXDIM), SCALE
      CHARACTER*(1)	TI
      INTEGER		RADD, DADD, PADD, NAXR, NAXISR(SYSMXDIM)
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
         IF (.NOT. DATEXIST(RDISP)) THEN
            NAXR = 1
            NAXISR(1) = NBINS
            DO 22 I = 2, SYSMXDIM
               NAXISR(I) = 1
 22         CONTINUE
            CALL DATMAKAR (RDISP, NAXR, NAXISR, 'R', DADD)
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
	 NX= NAXISI(1)
         CALL ARRRMEAN (MEMR(IADD), MEMR(RADD), MEMR(DADD), MEMR(PADD), 
     $        NAXISI, RPIXI, NBINS, SCALE, SHAPE, NX)
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

