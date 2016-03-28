C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)bpd.f	1.2	 24 Feb 1995
C
      SUBROUTINE SDEMAIN
C
CD Estimates the magnitude of the squint problem
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C					M.A.Holdaway	Nov 17 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'BPD')
C
      REAL			BW, DDRIFT, BPRIP, D2R
      INTEGER			NCHAN, ICHAN
      REAL	        	PI
      PARAMETER                 (PI=3.14159265358979)
      INTEGER		        NDUMMY
C
      REAL                      DR, DI
      COMPLEX			D1, D2, DDIFF
C
C==================================================================
      CALL MSGWELCO ('I estimate BP ripples effect on D terms')
 1    CONTINUE
      CALL USRCTL
      D2R = PI / 180.0
C
C Get input parameters
C
      CALL USRGETR ('BW', BW, 1, NDUMMY)
      CALL USRGETI ('NChan', NCHAN, 1, NDUMMY)
      CALL USRGETR ('DDRIFT', DDRIFT, 1, NDUMMY)
      CALL USRGETR ('BPRIP', BPRIP, 1, NDUMMY)
C
      D1 = CMPLX(0.0, 0.0)
      D2 = CMPLX(0.0, 0.0)
C
      DO 100 ICHAN = 1, NCHAN
         DR = COS( DDRIFT * D2R * FLOAT(ICHAN)) *
     $      (1.0 + BPRIP * 
     $      SIN( (BW/3.3) * FLOAT(ICHAN)/FLOAT(NCHAN) * 2.0 * PI))
         DI = SIN( DDRIFT * D2R * FLOAT(ICHAN)) *
     $      (1.0 + BPRIP * 
     $      SIN( (BW/3.3) * FLOAT(ICHAN)/FLOAT(NCHAN) * 2.0 * PI))
         D1 = D1 + CMPLX(DR, DI)
C
         DR = COS( DDRIFT * D2R * FLOAT(ICHAN)) *
     $      (1.0 - BPRIP * 
     $      SIN( (BW/3.3) * FLOAT(ICHAN)/FLOAT(NCHAN) * 2.0 * PI))
         DI = SIN( DDRIFT * D2R * FLOAT(ICHAN)) *
     $      (1.0 - BPRIP * 
     $      SIN( (BW/3.3) * FLOAT(ICHAN)/FLOAT(NCHAN) * 2.0 * PI))
         D2 = D2 + CMPLX(DR, DI)
 100  CONTINUE
C
      D1 = D1 / FLOAT(NCHAN)
      D2 = D2 / FLOAT(NCHAN)
      DDIFF = D1 - D2
      WRITE (MESSAGE, 1010) D1, D2, DDIFF
 1010 FORMAT ('D1 = ', 2F10.5,'   D2 = ', 2F10.5, '   Diff = ',2F10.5)
      CALL MSGPUT (MESSAGE, 'I')
C
      GOTO 1
C
 999  CONTINUE
      END
