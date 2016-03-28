C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlgconv.f	1.1    5/16/94
C
      SUBROUTINE UTLGCONV (INBEAM, TAPER, OUTBEAM)
C
C Calculate parameters of gaussian convolutions  (2-D only)
C
C	INBEAM	R(4)	input	Input Beam parameters
C	TAPER	R(4)	input	Tapering parameters
C	OUTBEAM	R(4)	input	Result of convolution
C
C All beams are BMAJ, BMIN, BPA, with the position angle of the
C BMAJ axis clockwise from north
C
C Deconvolution can be done by simply negating TAPER(1) and TAPER(2)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Oct 18 1993
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      REAL	INBEAM(4), TAPER(4), OUTBEAM(4)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLGCONV')
C
      REAL	PI, D2R
      PARAMETER	(PI=3.14159265359)
      PARAMETER (D2R=PI/180.0)
C
      REAL	AI, BI, CI, AT, BT, CT, AO, BO, CO,
     $   	THI, THT, THO, RA, RB, RC, BMAJ2, BMIN2
      REAL	XC, YC, SMAJ, SMIN, ROT
C=======================================================================
      IF (ERROR) GO TO 999
C
      IF ((INBEAM(1).EQ.0.0).AND.(INBEAM(2).EQ.0.0)) THEN
         OUTBEAM(1) = TAPER(1)
         OUTBEAM(2) = TAPER(2)
         OUTBEAM(3) = TAPER(3)
         OUTBEAM(4) = 0.0
         GO TO 999
      ELSE IF ((TAPER(1).EQ.0.0).AND.(TAPER(2).EQ.0.0)) THEN
         OUTBEAM(1) = INBEAM(1)
         OUTBEAM(2) = INBEAM(2)
         OUTBEAM(3) = INBEAM(3)
         OUTBEAM(4) = 0.0
         GO TO 999
      END IF
C
      THI = (INBEAM(3) + 90.0) * D2R
      BMAJ2 = SIGN((INBEAM(1)/2.0)**2,INBEAM(1))
      BMIN2 = SIGN((INBEAM(2)/2.0)**2,INBEAM(2))
C
      AI = (COS(THI)**2 / BMAJ2 + SIN(THI)**2 / BMIN2) / LOG(2.0)
      BI = (1.0 / BMAJ2 - 1.0 / BMIN2) * SIN(2.0*THI) / LOG(2.0) 
      CI = (SIN(THI)**2 / BMAJ2 + COS(THI)**2 / BMIN2) / LOG(2.0)
C
      THT = (TAPER(3) + 90.0) * D2R
      BMAJ2 = SIGN((TAPER(1)/2.0)**2,TAPER(1))
      BMIN2 = SIGN((TAPER(2)/2.0)**2,TAPER(2))
C
      AT = (COS(THT)**2 / BMAJ2 + SIN(THT)**2 / BMIN2) / LOG(2.0)
      BT = (1.0 / BMAJ2 - 1.0 / BMIN2) * SIN(2.0*THT) / LOG(2.0)
      CT = (SIN(THT)**2 / BMAJ2 + COS(THT)**2 / BMIN2) / LOG(2.0)
C
      RA = AI/(4.*AI*CI - BI**2) + AT/(4.*AT*CT - BT**2)
      RB = BI/(4.*AI*CI - BI**2) + BT/(4.*AT*CT - BT**2)
      RC = CI/(4.*AI*CI - BI**2) + CT/(4.*AT*CT - BT**2)
C
      AO = 1.0 / (4.*RC - RB**2/RA)
      BO = AO * RB / RA
      CO = AO * RC / RA
C
      CALL UTLQ2EL (AO, BO, CO, 0.0, 0.0, -1.0/LOG(2.0),
     $   XC, YC, SMAJ, SMIN, ROT)
C
      IF (ERROR) THEN
         CALL ERRCANCE
         CALL ERRREPOR (ERRFATAL, ROUTINE,
     $      'Can not (de)convolve given parameters')
         GO TO 999
      END IF
C
      OUTBEAM(1) = 2.0 * SMAJ
      OUTBEAM(2) = 2.0 * SMIN
      OUTBEAM(3) = ROT + 90.0
      OUTBEAM(4) = 0.0
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
