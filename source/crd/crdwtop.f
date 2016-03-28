C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdwtop.f	1.5    7/11/93
C
      SUBROUTINE CRDWTOP (NAME, WORLD, PIXEL)
C
CD Convert World to Pixel coordinates. Slow: do not use for each pixel.
C Knows about some non-linear coordinates.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	WORLD	REAL	input	World coordinates
C	PIXEL	REAL	output	Pixel coordinates
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added ARC projection
C				M.A.Holdaway	April 2 1993
C	Don't worry about zero DELT on pseudo axes
C				D.S.Briggs	26 June 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
      REAL		WORLD(*), PIXEL(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDWTOP')
C
      INTEGER           IAX, NAX, NAXIS(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      DOUBLE PRECISION	DWORLD(SYSMXDIM)
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 5 IAX = 1, SYSMXDIM
         PIXEL(IAX) = 0.0
   5  CONTINUE
C
      CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GO TO 990
C
      DO 1 IAX = 1, NAX
         DWORLD(IAX) = WORLD(IAX)
  1   CONTINUE
C
C Do those non-linear conversions we know about
C
      IF ((TYPE(1).EQ.'RA---SIN').AND.(TYPE(2).EQ.'DEC--SIN')) THEN
         CALL CRDSIN(RVAL(1), RVAL(2), DBLE(WORLD(1)), DBLE(WORLD(2)), 
     1      DWORLD(1), DWORLD(2))
      ELSE IF ((TYPE(1).EQ.'RA---ARC').AND.(TYPE(2).EQ.'DEC--ARC')) THEN
         CALL CRDARC(RVAL(1), RVAL(2), DBLE(WORLD(1)), DBLE(WORLD(2)), 
     1      DWORLD(1), DWORLD(2))
      END IF
C
      DO 10 IAX = 1, NAX
         IF (DELT(IAX).EQ.0.0) THEN
            IF (NAXIS(IAX).NE.1) THEN
               CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Zero DELT on axis')
               GO TO 999
            ELSE
               PIXEL(IAX) = RPIX(IAX)
            END IF
         ELSE
            PIXEL(IAX) = RPIX(IAX) + (DWORLD(IAX) - RVAL(IAX))/DELT(IAX)
         END IF
  10  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
