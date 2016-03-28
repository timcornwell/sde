C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdc2gal.f	1.1    10/2/91
C
      SUBROUTINE CRDC2GAL ( CRA, CDEC, GLONG, GLAT)
C
CD Converts Celestial RA and DEC to Galactic Longitude and Latitude
C
C	NAME	CH*(*)	input	Name of directory entry
C	CRA	DBLE	in	Celestial RA
C	CDEC	DBLE	in	Celestial DEC
C	GLONG	DBLE	out	Galactic Longitude, deg
C	GLAT	DBLE	out	Galactic latitude, deg
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Oct 2 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      DOUBLE PRECISION	GLONG, GLAT, CRA, CDEC
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDC2GAL')
C
      DOUBLE PRECISION	SINGLAT, SINLO33, D2R, PI
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      PI = ATAN2 (1.D0, 1.D0) * 4.D0
      D2R = PI / 180.D0
C
      SINGLAT = SIN(D2R*CDEC)*COS(D2R*62.6D0)  - COS(D2R*CDEC)*
     $   SIN(D2R*(CRA-282.25D0))*SIN(D2R*62.6)
      GLAT = ASIN(SINGLAT)/D2R
C
      IF (GLAT .EQ. 90.D0  .OR.  GLAT .EQ. -90.D0) THEN
         GLONG = 0.0
      ELSE
         SINLO33 = (COS(D2R*CDEC)*SIN(D2R*(CRA-282.25D0))*
     $      COS(D2R*62.6D0)  +  SIN(D2R*CDEC)*SIN(D2R*62.6D0))
     $      / COS ( D2R * GLAT )
         GLONG = ASIN (SINLO33)/D2R + 33.D0
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
