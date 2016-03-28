C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hanoise.f	1.2	24 Feb 1995
C
      SUBROUTINE SDEMAIN
C
CD Program to calculate noise vs HA
C
C Audit trail:
C				M.A.Holdaway	March 12 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HANOISE')
C
      REAL		HA(500), NOISE(500), NOISE2(500), EL2(500)
      REAL		RATE(500)
      REAL		SIG2
      INTEGER		I, J, N, NDUMMY, M
      CHARACTER*(SYSMXNAM)	DEVICE, TITLE
      REAL		DEC, LAT, ELMIN, HALIM, DHA, AFACT, EL
      REAL		CS90EL, D2R, SCAL
      
C==================================================================
C
      CALL MSGWELCO ('I calculate noise vs HA of an OBS')
 1    CONTINUE
      CALL USRCTL
C
      CALL USRGETR ('HA', HALIM, 1, NDUMMY)
      CALL USRGETR ('DeltaHA', DHA, 1, NDUMMY)
      CALL USRGETR ('Dec', DEC, 1, NDUMMY)
      CALL USRGETR ('Lat', LAT, 1, NDUMMY)
      CALL USRGETR ('Elmin', ELMIN, 1, NDUMMY)
      CALL USRGETR ('AFACT', AFACT, 1, NDUMMY)
C AFACT: noise at zenith = (1 + AFACT)
      CALL USRGETC ('Dev', DEVICE, 1, NDUMMY)
C
      D2R = ATAN2(1.0, 1.0) / 45.0
      HALIM = ABS(HALIM)
      N =  MAX( HALIM / DHA, 2.0)
      IF (N .GT. 500) THEN
         N = 500
         DHA = HALIM / 500.0
      ENDIF
C
      M = N
      DO 100 I = 1, N
         HA(I) = I * DHA
         CS90EL = COS(D2R*HA(I)*15.0) * COS(D2R*DEC) * COS(D2R*LAT) +
     $            SIN(D2R*DEC) * SIN(D2R*LAT)
         EL = 90. - ACOS(CS90EL)/D2R
         EL2(I) = EL
         IF (EL .LT. ELMIN) THEN
            M = I - 1
            GOTO 105
         ENDIF
         NOISE(I) = 1.0 + AFACT /SIN (D2R * EL)
 100  CONTINUE
 105  CONTINUE
C
C calculate relative noise out to that HA
C
      DO 200 I = 1, M
         SIG2 = 0.0
         DO 190 J = 1, I
            SIG2 = SIG2 + 1./NOISE(J)**2
 190     CONTINUE
         NOISE2(I) = 1./SQRT(SIG2)
 200  CONTINUE
C
      SCAL = 1./NOISE2(1)
      DO 290 I = 1, M
         NOISE2(I) = NOISE2(I) * SCAL
         WRITE (MESSAGE, 1965) HA(I), EL2(I), NOISE(I), NOISE2(I)
 1965    FORMAT( 4F10.5 )
         CALL MSGPUT (MESSAGE, 'D')
 290  CONTINUE
C
      DO 300 I = 2, M
         RATE(I-1) = (LOG10(NOISE2(I)) - LOG10(NOISE2(I-1)) ) /
     $                (LOG10(HA(I))   - LOG10(HA(I-1)))
 300  CONTINUE
C
C      WRITE (TITLE, 1066) LAT, DEC, AFACT
C 1066 FORMAT ('Lat = ',F5.0,' Dec = ',F5.0,' Afact = ',F6.3)
      TITLE = DEVICE
C
      N = M - 1
      CALL PIXPGGRF (N, HA, RATE, 0.0, 0.0, 0.0, 0.0,
     $   DEVICE, 'Hour Angle', 'Logrithmic Noise Rate', TITLE, 0, 0)
C
      GOTO 1
 999  CONTINUE
      END



