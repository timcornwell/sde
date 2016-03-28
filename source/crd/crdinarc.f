C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdinarc.f	1.1	 4/2/93
C
      SUBROUTINE CRDINARC (RA0, DEC0, RAP, DECP, RAS, DECS)
C
CD From an ARC projection, computes true RA and DEC.
C CRDINARC is inverse of CRDARC
C
C *** NB: ALL ANGLES ARE IN DEGREES *** ALL ARGUMENTS ARE DOUBLE PREC ***
C
C Arguments : CALL CRDSIN (RA0, DEC0, RAP, DECP, RAS, DECS)
C       RA0    DBLE     input  Coordinate reference right ascension of
C                               of arc projection
C       DEC0   DBLE     input  Coordinate reference declination of
C                               of arc projection
C       RAP     DBLE     input  right ascension on ARC projection
C       DECP    DBLE     input  declination on ARC projection
C       RAS     DBLE    output  RA on SKY
C       DECS    DBLE    output  Dec on SKY
C Audit trail:
C       Original version: Audit trail comments go on this line
C       and successive lines
C                               M.A.Holdaway	April 2, 1993
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      DOUBLE PRECISION	RA0, DEC0, RAP, DECP, RAS, DECS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDINARC')
C
      DOUBLE PRECISION	D2R, DXR, DYR, DEC0R, RA0R, ARG2, ARG1
      DOUBLE PRECISION	ARCR, SARCR
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C     
      D2R = 4.D0 * DATAN2(1.D0, 1.D0) / 180.D0
      DXR = (RAP  - RA0) * D2R
      DYR = (DECP - DEC0) * D2R
      DEC0R = DEC0 * D2R
      RA0R  = RA0  * D2R
      ARCR  = SQRT( DXR**2 + DYR**2 )
      IF (ARCR .EQ. 0) THEN
         SARCR = 1.0
      ELSE
         SARCR = DSIN ( ARCR ) / ARCR
      ENDIF
C
      ARG1 = DYR * DCOS( DEC0R ) * SARCR   +
     $       DSIN( DEC0R ) * DCOS(ARCR)
      DECS = DASIN (ARG1) / D2R
C
      ARG2 = SARCR * DXR / DCOS(DECS*D2R)
      RAS = DASIN (ARG2) /D2R + RA0
C
C      ARG1 = SQRT (1.D0 - DXR*DXR - DYR*DYR)
C      DECS  = (DASIN ( DYR*DCOS(DEC0R) + DSIN(DEC0R) * ARG1 ) )/D2R
C
C      ARG2 = DCOS(DEC0R) * ARG1 - DYR * DSIN(DEC0R) 
C      RAS = (DATAN2 (DXR, ARG2) ) / D2R + RA0
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
C
