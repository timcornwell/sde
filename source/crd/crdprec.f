C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdprec.f	1.3    11/7/90
C
      SUBROUTINE CRDPREC (RAI, DECI, EPI, EPF, RAF, DECF)
C
CD Precess coordinates between two epochs
C
C	RAI		DBLE	input	Initial Right Ascension (degrees)
C	DECI		DBLE	input	Initial Declination (degrees)
C	EPI		DBLE	input	Initial epoch e.g. 1989.453D0
C	EPF		DBLE	input	Final epoch e.g. 1950.0D0
C	RA		DBLE	output	Final Right Ascension (degrees)
C	DEC		DBLE	output	Final Declination (degrees)
C
C Audit trail:
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      DOUBLE PRECISION	RAI, DECI, EPI, EPF, RAF, DECF
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDPREC')
C
      DOUBLE PRECISION	RAT, DECT, D2R
      DATA		D2R	/0.017453293/
C===================================================================
C
      IF (ERROR) GO TO 999
C
C We work in degrees
C
      RAT = D2R * RAI
      DECT = D2R * DECI
      CALL SLAPRECES ('FK5', EPI, EPF, RAT, DECT)
      RAF = RAT / D2R
      DECF = DECT / D2R
C
 999  CONTINUE
      END




