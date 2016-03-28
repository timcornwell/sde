C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixclis1.f	1.1    6/10/93
C
      SUBROUTINE PIXCLIS1 (AL, XL, NL, RX, DX, CCFILE)
C
CD List pixels to an ASCII file in format:
C
C	FLUX(Jy)	OFFX(arcseconds)
C
C using free format io i.e. write (,*). The offset is with respect
C to the reference pixel.
C
C
C	AL	REAL(*)	input	List of values
C	XL	INT(*)	input	X list
C	NL	INT	input	Number of elements in list
C	RX	REAL	input	Reference pixels
C	DX	REAL	input	Coordinate increments (degrees)
C	CCFILE	CH*(*)	input	SDE handle of file to write to
C Audit trail:
C	Cloned from PIXCLIS2
C				D.S.Briggs	Oct 21 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NL, XL(*)
      REAL		AL(*), DX, RX
      CHARACTER*(*)	CCFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXCLIS1')
C
      INTEGER		ICOMP
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Load pixels into list
C
      DO 10 ICOMP = 1, NL
         IF(AL(ICOMP).EQ.0.0) GO TO 11
         WRITE (MESSAGE, 1000) AL(ICOMP), 
     1      3600.0 * DX * (FLOAT(XL(ICOMP)) - RX), ICOMP
 1000    FORMAT (2(2X, 1PG15.7E2), ' / ', I10)
         CALL TXTWRITE (CCFILE, MESSAGE)
         IF (ERROR) GO TO 990
  10  CONTINUE
  11  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
