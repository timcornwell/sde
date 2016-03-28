C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crddptow.f	1.3    6/29/94
C
      SUBROUTINE CRDDPTOW (NAME, PIXEL, WORLD)
C
CD Convert World from Pixel coordinates. Slow: do not use for each pixel.
C
C	NAME	CH*(*)	input	Name of directory entry
C	WORLD	DBLE	input	World coordinates
C	PIXEL	REAL	output	Pixel coordinates
C Audit trail:
C	Double precision version cloned from CRDPTOW
C				D.S.Briggs	May 5 1993
C	Calculate through NAX again, but this time don't complain
C	about zero DELT on pseudo axes
C				D.S.Briggs	June 10 1994
C	Do it right this time
C				D.S.Briggs	June 29 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
      DOUBLE PRECISION	WORLD(*)
      REAL		PIXEL(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDDPTOW')
C
      INTEGER           IAX, NAX, NAXIS(SYSMXDIM), RNAX
      DOUBLE PRECISION  RVAL(SYSMXDIM), DWORLD(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), 
     1			ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
C
      INTEGER		CRDRNAX
C=====================================================================
      IF (ERROR) GO TO 999
C
      DO 5 IAX = 1, SYSMXDIM
         WORLD(IAX) = 0.0
   5  CONTINUE
C
      CALL DATGETC (NAME, 'CTYPE', TYPE, SYSMXDIM, NAX)
      CALL DATGETI (NAME, 'ARRAY/NAXIS', NAXIS, SYSMXDIM, NAX)
      CALL DATGETR (NAME, 'CRPIX', RPIX, NAX, IAX)
      CALL DATGETD (NAME, 'CRVAL', RVAL, NAX, IAX)
      CALL DATGETR (NAME, 'CDELT', DELT, NAX, IAX)
      IF (ERROR) GO TO 999
C
      RNAX = CRDRNAX(NAX, NAXIS)
      DO 10 IAX = 1, NAX
         IF ((DELT(IAX).EQ.0.0).AND.(IAX.LE.RNAX)) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Zero DELT on axis')
            GO TO 999
         ELSE
            WORLD(IAX) = RVAL(IAX) + DELT(IAX)*(PIXEL(IAX) - RPIX(IAX))
         END IF
  10  CONTINUE
C
      DO 20 IAX = 1, RNAX
         DWORLD(IAX) = WORLD(IAX)
 20   CONTINUE
C
C Do those non-linear conversions we know about
C
      IF ((TYPE(1).EQ.'RA---SIN').AND.(TYPE(2).EQ.'DEC--SIN')) THEN
         CALL CRDINSIN(RVAL(1), RVAL(2), WORLD(1), WORLD(2), 
     $      DWORLD(1), DWORLD(2))
         WORLD(1) = DWORLD(1)
         WORLD(2) = DWORLD(2)
      ELSE IF (TYPE(1).EQ.'RA---ARC' .AND. TYPE(2).EQ.'DEC--ARC') THEN
         CALL CRDINARC(RVAL(1), RVAL(2), WORLD(1), WORLD(2), 
     $      DWORLD(1), DWORLD(2))
         WORLD(1) = DWORLD(1)
         WORLD(2) = DWORLD(2)
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
