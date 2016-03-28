C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdarc.f	1.2	 4/2/93
C
      SUBROUTINE CRDARC (RA0, DEC0, RA, DEC, RAC, DECC)
C
CD Computes the RA, DEC on an ARC projection for a real RA, DEC
C
C *** NB: ALL ANGLES ARE IN DEGREES ***
C
C Arguments : CALL CRDARC (RA0, DEC0, RA, DEC, RAC, DECC)
C	RA0	DBLE	input	Coordinate reference right ascension of
C				 of ARC projection
C	DEC0	DBLE	input	Coordinate reference declination of
C			 	 of ARC projection
C	RA	DBLE	input	Right ascension of sky position
C	DEC	DBLE	input	Declination of target point
C	RAC	DBLE	output  RA of target point in ARC-projection
C	DECC    DBLE	output  Dec of traget point in ARC-projection
C				Eg. coord in RA---ARC space.
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	April 2 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      DOUBLE PRECISION    D2R, RA0, DEC0, RA, DEC, RAC, DECC
      DOUBLE PRECISION	  THETA, THETSTHE, ARG1, DELTRA
C-----------------------------------------------------------------------
      IF (ERROR) GO TO 999
C
      D2R = 4.D0* ATAN(1.D0) / 180.D0
C
      DELTRA = RA-RA0
      ARG1 = DSIN(DEC*D2R) * DSIN(DEC0*D2R) +
     $   DCOS(DEC*D2R)*DCOS(DEC0*D2R)*DCOS(DELTRA*D2R)
      THETA = DACOS(ARG1)
      IF (THETA .EQ. 0.0) THEN
         THETSTHE = 1.0
      ELSE
         THETSTHE = THETA / DSIN(THETA)
      ENDIF
C
      RAC = RA0 + (THETSTHE * DCOS(DEC*D2R) * DSIN(DELTRA * D2R))/D2R
      DECC = DEC0 + (THETSTHE * ( DSIN(DEC*D2R) * DCOS(DEC0*D2R) -
     $   DCOS(DEC*D2R) * DSIN(DEC0*D2R) * DCOS(DELTRA*D2R) ) ) /D2R
C
 999  CONTINUE
      END
