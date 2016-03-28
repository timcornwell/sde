C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdae2rd.f	1.3    11/7/90
C
      SUBROUTINE CRDAE2RD (AZ, EL, LAT, LON, TIME, RA, DEC)
C
CD Get RA, DEC given AZ, EL, LAT, and TIME
C
C       AZ              REAL    input   Azimuth (degrees)
C       EL              REAL    input   Elevation (degrees)
C       LAT             REAL    input   Latitude of location (degrees)
C       TIME            DBLE    input   Longitude of location (degrees)
C       DAY             REAL    input   MJD of observation
C       UT              REAL    input   Universal Time (degrees)
C	RA		DBLE	output	Right Ascension (degrees)
C	DEC		DBLE	output	Declination (degrees)
C
C Audit trail:
C				R. T. Duquet	May 15 1990
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      DOUBLE PRECISION  AZ, EL
      DOUBLE PRECISION  LAT, LON
      DOUBLE PRECISION  RA, DEC
      DOUBLE PRECISION	TIME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDAE2RD')
C
      DOUBLE PRECISION  COSLAT, SINLAT, COSEL, SINEL, COSAZ, SINAZ
      DOUBLE PRECISION  HA, LST, D2R, SINHA, COSHA, SINDEC
      DOUBLE PRECISION	SLAGMST
C===================================================================
C
      IF (ERROR) GO TO 999
C
C We work in degrees
C
      D2R = ATAN(1.0D0)/45.0D0
C
      COSLAT = COS(D2R*LAT)
      SINLAT = SIN(D2R*LAT)
C
      COSAZ = COS(D2R*AZ)
      SINAZ = SIN(D2R*AZ)
      COSEL = COS(D2R*EL)
      SINEL = SIN(D2R*EL)
      SINDEC = SINLAT * SINEL + COSLAT * COSEL * COSAZ
      DEC = ASIN(SINDEC) / D2R
C
C These next two terms actually ignore cos(dec) which cancels
C
      COSHA = (COSLAT * SINEL - SINLAT * COSEL * COSAZ)
      SINHA = - COSEL * SINAZ
      HA = ATAN2(SINHA, COSHA) / D2R
C
      LST = SLAGMST(TIME) / D2R - LON
      RA = MOD(LST - HA, 360.0D0)
C
 999  CONTINUE
      END




