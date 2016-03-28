C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdsin.f	1.4    3/11/92
C
      SUBROUTINE CRDSIN (RA0, DEC0, RA, DEC, RAC, DECC)
C
CD Computes the RA, DEC on a SIN projection for a real RA, DEC
C
C *** NB: ALL ANGLES ARE IN DEGREES ***
C
C Arguments : CALL CRDSIN (RA0, DEC0, RA, DEC, RAC, DECC)
C	RA0	DBLE	input	Coordinate reference right ascension of
C				 of sin projection
C	DEC0	DBLE	input	Coordinate reference declination of
C			 	 of sin projection
C	RA	DBLE	input	Right ascension of sky position
C	DEC	DBLE	input	Declination of target point
C	RAC	DBLE	output  RA of target point in sin-projection
C	DECC    DBLE	output  Dec of traget point in sin-projection
C				Eg. coord in RA---SIN space.
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Removed extra ASIN() call in conversion of L & M to RAC & DECC
C	Fixed minor problem with single precision constant.
C				D.S.Briggs	10 Mar 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      DOUBLE PRECISION    D2R, RA0, DEC0, RA, DEC, RAC, DECC, COSS, SINS
C-----------------------------------------------------------------------
      IF (ERROR) GO TO 999
C
      D2R = 4.D0* ATAN(1.D0) / 180.D0
      COSS = DCOS (D2R*DEC)
      SINS = DSIN (D2R*DEC)
      RAC = DSIN(D2R*(RA-RA0)) * COSS
      DECC = SINS * DCOS(D2R*DEC0) - COSS * DSIN(D2R*DEC0) * 
     1   DCOS(D2R*(RA-RA0))
      RAC = RA0 + RAC / D2R
      DECC = DEC0 + DECC / D2R
C
 999  CONTINUE
      END
