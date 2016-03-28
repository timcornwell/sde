C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdinsin.f	1.3    11/7/90
C
      SUBROUTINE CRDINSIN (RA0, DEC0, RAP, DECP, RAS, DECS)
C
CD From a SIN projection, computes true RA and DEC.
C CRDINSIN is inverse of CRDSIN
C
C *** NB: ALL ANGLES ARE IN DEGREES *** ALL ARGUMENTS ARE DOUBLE PREC ***
C
C Arguments : CALL CRDSIN (RA0, DEC0, RAP, DECP, RAS, DECS)
C       RA0    DBLE     input  Coordinate reference right ascension of
C                               of sin projection
C       DEC0   DBLE     input  Coordinate reference declination of
C                               of sin projection
C       RAP     DBLE     input  right ascension on SIN projection
C       DECP    DBLE     input  declination on SIN projection
C       RAS     DBLE    output  RA on SKY
C       DECS    DBLE    output  Dec on SKY
C Audit trail:
C       Original version: Audit trail comments go on this line
C       and successive lines
C                               M.A.Holdaway	May 9 1990
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      DOUBLE PRECISION	RA0, DEC0, RAP, DECP, RAS, DECS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDINSIN')
C
      DOUBLE PRECISION	D2R, DXR, DYR, DEC0R, RA0R, ARG2, ARG1
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
C
      ARG1 = SQRT (1.D0 - DXR*DXR - DYR*DYR)
      DECS  = (DASIN ( DYR*DCOS(DEC0R) + DSIN(DEC0R) * ARG1 ) )/D2R
C
      ARG2 = DCOS(DEC0R) * ARG1 - DYR * DSIN(DEC0R) 
      RAS = (DATAN2 (DXR, ARG2) ) / D2R + RA0
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
