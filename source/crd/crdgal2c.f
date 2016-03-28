C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdgal2c.f	1.1    10/2/91
C
      SUBROUTINE CRDGAL2C (GLONG, GLAT, CRA, CDEC)
C
CD Converts Galactic Longitude and Latitude to Celestial RA and DEC
C
C	NAME	CH*(*)	input	Name of directory entry
C	GLONG	DBLE	input	Galactic Longitude, deg
C	GLAT	DBLE	input	Galactic latitude, deg
C	CRA	DBLE	out	Celestial RA
C	CDEC	DBLE	out	Celestial DEC
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
      PARAMETER		(ROUTINE = 'CRDGAL2C')
C
      DOUBLE PRECISION	SINDEC, SINRA282, D2R, PI
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      PI = ATAN2 (1.D0, 1.D0) * 4.D0
      D2R = PI / 180.D0
C
      SINDEC = COS (D2R*GLAT) * SIN(D2R*(GLONG - 33.D0))*SIN(D2R*62.6D0)
     $   + SIN(D2R*GLAT)*COS(D2R*62.6D0)
      CDEC = ASIN(SINDEC)/D2R
C
      IF (CDEC .EQ. 90.D0  .OR.  CDEC .EQ. -90.D0) THEN
         CRA = 0.0
      ELSE
         SINRA282 = ( COS (D2R*GLAT) * SIN(D2R*(GLONG - 33.D0))
     $      * COS(D2R*62.6D0) - SIN(D2R*GLAT)*SIN(D2R*62.6D0) )
     $      / COS(D2R*CDEC)
         CRA = ASIN (SINRA282)/D2R + 282.25D0
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
