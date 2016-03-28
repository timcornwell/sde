C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdcorrp.f	1.1	 5/4/93
C
      SUBROUTINE VSDCORRP (
     $   IV, IW, QV, QW, UV, UW, VV, VW, 
     $   BASL, VTIME, STIME,
     $   NVIS, NANT, NUMDINT, NUMVINT, TINTD, DR, DL, DTIME,
     $   SEED, DRMS, DRIFTRMS, CFRAC, ACDRIFT, NRMS,
     $   IVN, IWN, QVN, QWN, UVN, UWN, VVN, VWN,
     $   RMSI, RMSQ, RMSU, RMSV,
     $   DATEOBS, OBSRA, OBSDEC, EPOCH, SLAT, SLON
     $   )
C
CD Corrupt IQUV vis with simulated D terms
C
C	IV	X(*)		inp	Input I visibilities
C	IW	R(*)		inp	Input I weights
C					same for Q, U, V
C	BASL	REAL(*)		input	Baselines
C	VTIME	REAL(*)		input	Times, in days
C	STIME	REAL(*)		input	Time, station oriented
C	NVIS	INT		input	Number of visibilities
C	NANT	INT		input	Number of antennas
C	NUMDINT	INT		input	Number of intervals for D terms
C	NUMVINT	INT		input	Number of intervals for visibilities
C	TINTD	REAL		input	Time interval for D terms, days
C	DR	X(NANT,NUMDINT)	output	Compex D_R
C	DL	X(NANT,NUMDINT)	output	Compex D_L
C	DTIME	REAL(*)		output	Time at which DR, DL are valid
C	SEED	INT		input	Random Number Seed
C	DRMS	REAL		input	RMS value for DR, DL (~.02)
C	DRIFTRMS REAL		input	RMS Drift in DR, DL, per day
C	CFRAC	REAL		input	Fraction of D which is correlated
C	ACDRIFT	REAL		input	AC Phase drift, in degrees
C	NRMS	REAL		input	Noise in Visibilities
C	
C	IVN	X(*)		out	Output I vis
C	IWN	R(*)		out	Output I weight
C					same for Q, U, V
C	RMSI	REAL		out	RMS error added to I
C					same for Q, U, V
C	DATEOBS	CH*(*)		in	'23/01/91'
C	OBSDEC	DBLE		in	Ibserved Dec, degrees
C	OBSRA	DBLE		in	Observed RA, degrees
C	EPOCH	REAL		in	Epoch of RA, DEC
C	SLAT	REAL		in	Site Latitude, degrees
C	SLON	REAL		in	Site Longitude, degrees
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Sept 5 1992
C	Now we calculate the paralactic angle from vtime,OBSRA,LAT,LON,
C	for a "small" interferometer.
C	VISSIMPD now gives us TIME which is accurate enough to result
C	in paralactic angle errors of about 2.5E-5 degrees per hour
C				M.A.Holdaway	Sept 22 1992
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NANT, NUMDINT, NUMVINT, SEED
      COMPLEX		IV(*), UV(*), QV(*), VV(*)
      COMPLEX		IVN(*), UVN(*), QVN(*), VVN(*)
      REAL		IW(*), UW(*), QW(*), VW(*)
      REAL		IWN(*), UWN(*), QWN(*), VWN(*)
      REAL		BASL(*), VTIME(*), STIME(*)
      COMPLEX		DR(NANT,*), DL(NANT,*)
      REAL		DTIME(*), TINTD
      REAL		DRMS, DRIFTRMS, NRMS, CFRAC, ACDRIFT
      REAL		RMSI, RMSQ, RMSU, RMSV
      CHARACTER*(*)	DATEOBS
      REAL		EPOCH, SLAT, SLON
      DOUBLE PRECISION	OBSRA, OBSDEC
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDCORRP')
C
      COMPLEX		RDRIFT, LDRIFT, DCORR, J
      COMPLEX		RR, LL, RL, LR, RR2, LL2, RL2, LR2, I, Q, U, V
      COMPLEX		EMC, EPC, EM, EP, ACPH, ACPHC
      REAL		PA1, PA2, D2R
      INTEGER		IVIS, IA1, IA2, INTNDX
      REAL		RNOISE, INOISE, SUMWT
      REAL		PI, RGAIN, IGAIN, TINTV
      PARAMETER		(PI=3.14159274101257)
      INTEGER		ISTART, IEND, NGOOD
      DOUBLE PRECISION	RANOW, DECNOW, MJD, SLAEPB, DATE, LST, SLAGMST,
     $   		SLATR, DECNOWR, HAR, AZ, EL, PA
C
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      ACPH = CMPLX (1.0, 0.0)
      ACPHC = CMPLX (1.0, 0.0)
      D2R = PI/180.0
      J = CMPLX(0.0,1.0)
C
C  Precess once
C
      CALL UTLD3MJD (DATEOBS, MJD)
      DATE = SLAEPB (MJD)
      CALL CRDPREC (OBSRA, OBSDEC, DBLE(EPOCH), DBLE(DATE),
     $   RANOW, DECNOW)
      DECNOWR = DECNOW*D2R
      SLATR = SLAT*D2R
C
C Make D terms
C
      IF (CFRAC .GE. 1.0) CFRAC = .999
C
      IF (CFRAC .GT. 0.0) THEN
         CALL UTLGRAND(RGAIN, SEED)
         DCORR = DRMS * CFRAC * CMPLX
     $      (COS (100. * RGAIN), COS (100. * RGAIN) )
     $      / SQRT(2.0)
      ELSE
         DCORR = 0.0
      ENDIF


      DO 31 IA1 = 1, NANT
         CALL UTLGRAND(RGAIN, SEED)
         CALL UTLGRAND(IGAIN, SEED)
         DR(IA1, 1) = DRMS * (1.0-CFRAC) * CMPLX(RGAIN, IGAIN)
     $      /SQRT(2.0) + DCORR
         CALL UTLGRAND(RGAIN, SEED)
         CALL UTLGRAND(IGAIN, SEED)
         DL(IA1, 1) = DRMS * (1.0-CFRAC) * CMPLX(RGAIN, IGAIN)
     $      /SQRT(2.0) + CONJG (DCORR)
C
C use these values for debugging
C
C         DR(IA1, 1) = CMPLX(DRMS, DRMS)
C         DL(IA1, 1) = CMPLX(DRMS, -DRMS)
C
 31   CONTINUE
      DO 41 IA1 = 1, NANT
         CALL UTLGRAND(RGAIN, SEED)
         CALL UTLGRAND(IGAIN, SEED)
         RDRIFT = DRIFTRMS * TINTD * CMPLX(RGAIN, IGAIN)
         CALL UTLGRAND(RGAIN, SEED)
         CALL UTLGRAND(IGAIN, SEED)
         LDRIFT = DRIFTRMS * TINTD * CMPLX(RGAIN, IGAIN)
         DO 37 INTNDX = 2, NUMDINT
            DR(IA1, INTNDX) = DR(IA1, INTNDX-1) + RDRIFT
            DL(IA1, INTNDX) = DL(IA1, INTNDX-1) + LDRIFT
 37      CONTINUE
 41   CONTINUE
C
C ********************** Start of loop over integrations *************
C
      SUMWT = 0.0
      RMSI = 0.0
      RMSQ = 0.0
      RMSU = 0.0
      RMSV = 0.0
      INTNDX = 1
      ISTART = 1
      IEND = 1
      CALL PIXMDIFF (VTIME, NVIS, TINTV)
C
  1   CONTINUE
C
C Loop over data for this integration
C
      NGOOD = 0
      DTIME(INTNDX) = 0.0
      DO 100 IVIS = ISTART, NVIS
         IF (IW(IVIS).LE.0.0) GO TO 100
         IF (DTIME(INTNDX).EQ.0.0) THEN
            DTIME(INTNDX) = VTIME(IVIS) + TINTD / 2.0
         END IF
         IF (ABS(VTIME(IVIS)-DTIME(INTNDX)+TINTV/2.0).LE.(TINTD/2.0)) 
     $      THEN
            NGOOD = NGOOD + 1
            IEND = IVIS
         ELSE
            IEND = IVIS - 1
            GO TO 120
         END IF
 100  CONTINUE
      IEND = NVIS
C
 120  CONTINUE
C
C If no good data then skip the rest of the steps
C
      IF (NGOOD.EQ.0) GO TO 310
C
C Go corrupt the data
C
      DO 300 IVIS = ISTART, IEND
         IWN(IVIS) = IW(IVIS)
         QWN(IVIS) = QW(IVIS)
         UWN(IVIS) = UW(IVIS)
         VWN(IVIS) = VW(IVIS)
         IF (IWN(IVIS).LE.0.0) THEN
            IVN(IVIS) = 0.
            QVN(IVIS) = 0.
            UVN(IVIS) = 0.
            VVN(IVIS) = 0.
C
            QWN(IVIS) = 0.
            UWN(IVIS) = 0.
            VWN(IVIS) = 0.
            GO TO 300
         END IF
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
C
         LST = SLAGMST( MJD + VTIME(IVIS))/D2R - SLON
         HAR = (LST - RANOW) * D2R
         CALL GETANGLS (HAR, DECNOWR, SLATR, AZ, EL, PA)
         PA1 = PA / D2R
         PA2 = PA / D2R
C
         EP = CMPLX(COS(D2R*(PA1+PA2)), SIN(D2R*(PA1+PA2)) )
         EM = CMPLX(COS(D2R*(PA1-PA2)), SIN(D2R*(PA1-PA2)) )
         EPC = CMPLX(COS(D2R*(PA1+PA2)), -SIN(D2R*(PA1+PA2)) )
         EMC = CMPLX(COS(D2R*(PA1-PA2)), -SIN(D2R*(PA1-PA2)) )
C
         RR = IV(IVIS) + VV(IVIS)
         LL = IV(IVIS) - VV(IVIS)
         RL = QV(IVIS) + J * UV(IVIS) 
         LR = QV(IVIS) - J * UV(IVIS) 
C
         CALL UTLGRAND(RNOISE, SEED)
         CALL UTLGRAND(INOISE, SEED)
         RR2 = RR * EMC
     $      + RL * CONJG(DR(IA2,INTNDX)) * EPC
     $      + LR * DR(IA1,INTNDX) *EP
     $      + LL * DR(IA1,INTNDX) * CONJG(DR(IA2,INTNDX)) * EM
     $      + NRMS * CMPLX(RNOISE, INOISE)
C
         CALL UTLGRAND(RNOISE, SEED)
         CALL UTLGRAND(INOISE, SEED)
         LL2 = LL * EM
     $      + LR * CONJG(DL(IA2,INTNDX)) * EP
     $      + RL * DL(IA1,INTNDX) * EPC
     $      + RR * DL(IA1,INTNDX) * CONJG(DL(IA2,INTNDX)) * EMC
     $      + NRMS * CMPLX(RNOISE, INOISE)
C
         CALL UTLGRAND(RNOISE, SEED)
         CALL UTLGRAND(INOISE, SEED)
         RL2 = RL * EPC
     $      + RR * CONJG(DL(IA2,INTNDX)) * EMC
     $      + LL * DR(IA1,INTNDX) * EM
     $      + LR * DR(IA1,INTNDX) * CONJG(DL(IA2,INTNDX)) * EP
     $      + NRMS * CMPLX(RNOISE, INOISE)
C
         CALL UTLGRAND(RNOISE, SEED)
         CALL UTLGRAND(INOISE, SEED)
         LR2 = LR * EP
     $      + LL * CONJG(DR(IA2,INTNDX)) * EM
     $      + RR * DL(IA1,INTNDX) * EMC
     $      + RL * DL(IA1,INTNDX) * CONJG(DR(IA2,INTNDX)) * EPC
     $      + NRMS * CMPLX(RNOISE, INOISE)
C
C Apply AC Phase
C
         RL2 = RL2 * ACPH
         LR2 = LR2 * ACPHC
C
         I = (LL2*EMC + RR2*EM)/2.0
         Q = (RL2*EP + LR2*EPC)/2.0
         U = J *(LR2*EPC - RL2*EP)/2.0
         V = (RR2*EM-LL2*EMC)/2.0
C
         RMSI = RMSI + CABS(IV(IVIS) - I)**2
         RMSQ = RMSQ + CABS(QV(IVIS) - Q)**2
         RMSU = RMSU + CABS(UV(IVIS) - U)**2
         RMSV = RMSV + CABS(VV(IVIS) - V)**2
         SUMWT = SUMWT + 1.0
C
         IV(IVIS) = I
         QV(IVIS) = Q
         UV(IVIS) = U
         VV(IVIS) = V
 300  CONTINUE
C
      IF (NUMDINT .GT. 1) THEN
         ACPH = CMPLX( COS(D2R*ACDRIFT*FLOAT(INTNDX)/FLOAT(NUMDINT-1)),
     $      SIN(D2R*ACDRIFT*FLOAT(INTNDX)/FLOAT(NUMDINT-1)) )
         ACPHC = CONJG (ACPH)
      ENDIF
      INTNDX = INTNDX + 1      
C
 310  CONTINUE
C
C Now move onto the next integration interval
C
      ISTART = IEND + 1
C
C Any data left?
C
      IF (ISTART.LE.NVIS) THEN
         IF (INTNDX.GT.NUMDINT) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Too many integrations')
            GO TO 999
         END IF
         GO TO 1
      END IF
C
C ********************** End of loop over integrations *****************
C
      IF (SUMWT.GT.0.0) THEN
         RMSI = SQRT(RMSI/SUMWT)
         RMSQ = SQRT(RMSQ/SUMWT)
         RMSU = SQRT(RMSU/SUMWT)
         RMSV = SQRT(RMSV/SUMWT)
      ELSE
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No data')
         GO TO 999
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
