C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsubse.f	1.5    6/7/93
C
      SUBROUTINE IMGSUBSE (A, B, WINDOW)
C
CD Subsection and Sum image, fix up the header
C
C	A	CH*(*)	input	Directory name of image
C	B	CH*(*)	input	Directory name of image
C	WINDOW	CH*(*)	input	Directory name of window
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Only copy header and history if B had to be created
C				T.J.Cornwell	April 11 1990
C	Added summing capability
C				R.G.Marson	Mar 4 1992
C	Bugfix to refpix calculation
C				D.S.Briggs	May 11 1993
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	A, B, WINDOW
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='IMGSUBSE')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE
      INTEGER		BLC(SYSMXDIM)
      INTEGER		STEP(SYSMXDIM)
      INTEGER		SUM(SYSMXDIM)
      CHARACTER		STRM2*(SYSMXNAM)
      INTEGER 		AADD, ANAX, ANAXIS(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM)
      INTEGER 		IAX, NDUMMY
      LOGICAL		DATEXIST, EXISTED
C=====================================================================
      IF (ERROR) GO TO 999
C
      EXISTED = DATEXIST(B)
C
C Make B if it did not exist
C
      IF(.NOT.EXISTED) THEN
         CALL DATCREAT (B)
      END IF
C
C Now actually call routine which does the work on the array
C here on data type of the array.
C
      CALL ARRSUBSE (A, B, WINDOW)
C
C Copy header and history
C
      IF(.NOT.EXISTED) THEN
         CALL HEDCOPY (A, B)
         CALL HISCOPY (A, B)
      END IF
C
C Fix increment and reference pixel
C
      CALL DATGETAR (A, ANAX, ANAXIS, ATYPE, AADD)
      IF (DATEXIST(STRM2(WINDOW, 'BLC'))) THEN
         CALL DATGETI (WINDOW, 'BLC', BLC, SYSMXDIM, NDUMMY)
      ELSE
         DO 10 IAX = 1, ANAX
           BLC(IAX) = 1
  10     CONTINUE
      END IF
      IF (DATEXIST(STRM2(WINDOW, 'STEP'))) THEN
         CALL DATGETI (WINDOW, 'STEP', STEP, SYSMXDIM, NDUMMY)
      ELSE
         DO 15 IAX = 1, ANAX
            STEP (IAX) = 1
  15     CONTINUE
      END IF
      IF (DATEXIST(STRM2(WINDOW, 'SUM'))) THEN
         CALL DATGETI (WINDOW, 'SUM', SUM, SYSMXDIM, NDUMMY)
      ELSE
         DO 17 IAX = 1, ANAX
            SUM (IAX) = 1
 17      CONTINUE
      END IF
      CALL DATGETR (A, 'CDELT', DELT, ANAX, NDUMMY)
      CALL DATGETR (A, 'CRPIX', RPIX, ANAX, NDUMMY)
      IF (ERROR) GO TO 990
C
C Note that the refpix will in general be non-integral
C
      DO 20 IAX = 1, ANAX
         RPIX(IAX) = (RPIX(IAX) - BLC(IAX))/STEP(IAX) + 1. +
     $      (1.-SUM(IAX))/(2.*STEP(IAX))
         IF (STEP(IAX).NE.0.0) THEN
            DELT(IAX) = DELT(IAX)*STEP(IAX)
         END IF
  20  CONTINUE
      CALL DATPUTR (B, 'CDELT', DELT, ANAX)
      CALL DATPUTR (B, 'CRPIX', RPIX, ANAX)
C
C Trace errors
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
