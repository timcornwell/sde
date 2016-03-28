C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)simaerd.f	1.3	 8/20/92
C
      SUBROUTINE SIMAERD (DEC, H, LAT, AZERR, ELERR, RAERR, DECERR)
C
C Convert pointing errors from AZ-EL to RA-DEC
C All input angles in degrees, except for HA (hours), LAT in RAD
C
C
C	DEC	DOUBL	inp	declination
C	H	DOUBL	inp	hour angle
C	LAT	DOUBL	inp	site latitude
C	AZERR	REAL	inp	error in azimuth
C	ELERR	REAL	inp	error in elevation
C	RAERR	REAL	inp	error in right ascension
C	DECERR	REAL	inp	error in declination
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Feb 12 1990
C	Conversion to RA has the wrond sign.
C	Never mattered until now, as the absolute pointing
C	has never been used, but that changes now
C				M.A.Holdaway	Aug 18 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL		AZERR, ELERR, RAERR, DECERR
      DOUBLE PRECISION	DEC, H, LAT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SIMAERD')
C
      DOUBLE PRECISION		AZ, EL, PAR, H2, DEC2, DEGTRAD
      DOUBLE PRECISION		H1, DEC1, LAT1, AZ1, EL1
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DEGTRAD = 4.D0 * ATAN2(1.D0,1.D0)/180.D0
      DEC1 = DEC * DEGTRAD
      LAT1 = LAT 
      H1   = H * 360.D0 / 24.D0 * DEGTRAD
C
      CALL GETANGLS (H1, DEC1, LAT1, AZ, EL, PAR)
      EL1 = EL + ELERR * DEGTRAD
      IF ( ABS (COS (EL1))  .GE. .0001) THEN
         AZ1 = AZ + ( AZERR * DEGTRAD )/ COS ( EL1 )
      ELSE
         AZ1 = AZ
      ENDIF
      CALL GETANGL2 (AZ1, EL1, LAT1, H2, DEC2)
C
      H2 = H2 / DEGTRAD
      H1 = H * 360.D0 / 24.D0
      DEC2 = DEC2 / DEGTRAD
      DECERR = DEC2 - DEC
      RAERR  = -(H2   - H1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
