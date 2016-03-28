C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdintrp.f	1.2	 2/24/95
C
      SUBROUTINE VSDINTRP (
     $   IV, IW, QV, QW, UV, UW, 
     $   BASL, VTIME, ROTATED,
     $   NVIS, NANT, NDINT, 
     $   DR, DL, DTIME, DRWT, DLWT,
     $   DATEOBS, OBSRA, OBSDEC, EPOCH, SLAT, SLON, INTERP)
C
CD Apply D terms to Q and U visibilities
C
C	IV	X(*)		in/out	Input I visibilities
C	IW	R(*)		in/out	Input I weights
C					same for Q, U
C	BASL	REAL(*)		input	Baselines
C	VTIME	REAL(*)		input	Times, in days
C	ROTATED	L		input	Paralactic angle corrections?
C	NVIS	INT		input	Number of visibilities
C	NANT	INT		input	Number of antennas
C	NDINT	INT		input	Number of intervals for D terms
C	DR	X(NANT,NDINT) in	DR
C	DL	X(NANT,NDINT) in	DL
C	DTIME	REAL(*)		output	Time at which DR, DL are valid
C	DRWT	REAL(NANT,NUMDI)  out	Wt: 1 for valid, 0 or less for invalid      
C	DLWT	REAL(NANT,NUMDI)  out	Wt: 1 for valid, 0 or less for invalid      
C       DATEOBS CH*(*)          in      '23/01/91'
C       OBSDEC  DBLE            in      Observed Dec, degrees
C       OBSRA   DBLE            in      Observed RA, degrees
C       EPOCH   REAL            in      Epoch of RA, DEC
C       SLAT    REAL            in      Site Latitude, degrees
C       SLON    REAL            in      Site Longitude, degrees
C	INTERP	CH(*)		in	Interpolation method:
C					[NEAREST | 2PT | ALL]
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	April 21 1993
C
C	Fixed a bug in applying the D terms
C				M.A. Holdaway	May 7 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NANT, NDINT
      COMPLEX		IV(*), UV(*), QV(*)
      REAL		IW(*), UW(*), QW(*)
      REAL		BASL(*), VTIME(*)
      COMPLEX		DR(NANT,*), DL(NANT,*)
      REAL		DTIME(*), DRWT(NANT,*), DLWT(NANT,*)
      CHARACTER*(*)     DATEOBS, INTERP
      REAL              EPOCH, SLAT, SLON
      DOUBLE PRECISION  OBSRA, OBSDEC
      LOGICAL		ROTATED
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDINTRP')
C
      COMPLEX		J, RLT, LRT, LRCT
      COMPLEX		RR, RL
      COMPLEX		LRC, RRC
      COMPLEX		EMC, EPC, EM, EP
      REAL		PA1, PA2, D2R
      INTEGER		IVIS, IA1, IA2
      REAL		PI
      PARAMETER		(PI=3.14159274101257)
      INTEGER		NR1, NR2, NL1, NL2, ID, ID2
      CHARACTER*1	MYINTERP
      REAL		WT1, WT2
      COMPLEX		DR1, DR2, DL1, DL2
      DOUBLE PRECISION  RANOW, DECNOW, MJD, SLAEPB, DATE, LST, SLAGMST,
     $                  SLATR, DECNOWR, HAR, AZ, EL, PA
C
      INTEGER		PIXRNEAR
C=====================================================================
C
C	variable nomenclature:
C
C	RR, RL, LR:            "rotated" measured visibilities
C       RRM, RLM, LRM:         "unrotated" model visibilities
C       RRMC, LRMC, RRC, LRC:   complex conjugate of quantites
C	EP                      e^{i(PA1 + PA2)}   "e plus"
C	EPC                     e^{-i(PA1 + PA2)}  "e plus, conjugate"
C	EM                      e^{i(PA1 - PA2)}   "e minus"
C	EMC                     e^{-i(PA1 - PA2)} "e minus, conjugate"
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      D2R = PI/180.0
      J = CMPLX(0.0,1.0)
C
C ********************** Start of loop over integrations *************
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
C Corrected  Q, U
C
      DO 500 IVIS = 1, NVIS
         IF (QW(IVIS).LE.0.0 .OR. UW(IVIS) .LE.0.0) THEN
            QW(IVIS) = 0.0
            UW(IVIS) = 0.0
            GO TO 500
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
         EP  = CMPLX(COS(D2R*(PA1+PA2)), SIN(D2R*(PA1+PA2)) )
         EPC = CMPLX(COS(D2R*(PA1+PA2)), -SIN(D2R*(PA1+PA2)) )
         EM  = CMPLX(COS(D2R*(PA1-PA2)), SIN(D2R*(PA1-PA2)) )
         EMC = CMPLX(COS(D2R*(PA1-PA2)), -SIN(D2R*(PA1-PA2)) )
C
C Note: RR, LL, RL, LRC here represent raw data visibilities,
C therefore affected by the paralactic angle phase shift.
C
         IF (ROTATED) THEN
            RR = IV(IVIS)*EMC
            RRC = CONJG ( RR )
            RL = (QV(IVIS) + J * UV(IVIS) ) * EPC
            LRC = CONJG ( (QV(IVIS) - J * UV(IVIS) ) * EP )
         ELSE
            RR = IV(IVIS)
            RRC = CONJG ( RR )
            RL = QV(IVIS) + J * UV(IVIS)
            LRC = CONJG  (QV(IVIS) - J * UV(IVIS) )
         ENDIF
C
C Find interpolated D terms
C
         MYINTERP = INTERP(1:1)
         IF (MYINTERP .EQ. '2') THEN
C            2pt interpolation; if time comes before first
C	     or after last D term time, then use 'NEAREST'
            ID = PIXRNEAR (DTIME, NDINT, VTIME(IVIS))
            IF (ID .LE. 0 .OR. ID .GT. NDINT) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     $            'D Term index out of range')
               GOTO 990
            ENDIF
            IF ((ID .EQ. 1 .AND. DTIME(1) .GE. VTIME(IVIS)) .OR.
     $         (ID.EQ.NDINT .AND. DTIME(NDINT).LE.VTIME(IVIS))) THEN
               MYINTERP = 'N'
               GOTO 482
            ENDIF
            NR1 = 0
            NR2 = 0
            NL1 = 0
            NL2 = 0
            IF (VTIME(IVIS) .LT. DTIME(ID)) ID = ID - 1
            ID2 = ID + 1
            WT1 = (DTIME(ID2) - VTIME(IVIS))/(DTIME(ID2) - DTIME(ID))
            WT2 = (VTIME(IVIS) - DTIME(ID))/(DTIME(ID2) - DTIME(ID))
            IF (DRWT(IA1,ID) .GT .0. .AND. DRWT(IA1,ID2) .GT. 0.) THEN
               NR1 = 1
               DR1 = DR(IA1, ID) * WT1 + DR(IA1, ID2) * WT2
            ENDIF
            IF (DRWT(IA2,ID) .GT. 0. .AND. DRWT(IA1,ID2) .GT. 0.) THEN
               NR2 = 1
               DR2 = DR(IA2, ID) * WT1 + DR(IA2, ID2) * WT2
            ENDIF
            IF (DLWT(IA1,ID) .GT. 0. .AND. DRWT(IA1,ID2) .GT. 0.) THEN
               NL1 = 1
               DL1 = DL(IA1, ID) * WT1 + DL(IA1, ID2) * WT2
            ENDIF
            IF (DLWT(IA2,ID) .GT. 0. .AND. DRWT(IA1,ID2) .GT. 0.) THEN
               NL2 = 1
               DL2 = DL(IA2, ID) * WT1 + DL(IA2, ID2) * WT2
            ENDIF
         ENDIF
 482     CONTINUE
         IF (MYINTERP .EQ. 'N') THEN
C	     nearest
            NR1 = 0
            NR2 = 0
            NL1 = 0
            NL2 = 0
            ID = PIXRNEAR (DTIME, NDINT, VTIME(IVIS))
            IF (ID .LE. 0 .OR. ID .GT. NDINT) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     $            'D Term index out of range')
               GOTO 990
            ENDIF
            IF (DRWT(IA1, ID) .GT. 0.0) THEN
               NR1 = 1
               DR1 = DR(IA1, ID)
            ENDIF
            IF (DRWT(IA2, ID) .GT. 0.0) THEN
               NR2 = 1
               DR2 = DR(IA2, ID)
            ENDIF
            IF (DLWT(IA1, ID) .GT. 0.0) THEN
               NL1 = 1
               DL1 = DL(IA1, ID)
            ENDIF
            IF (DLWT(IA2, ID) .GT. 0.0) THEN
               NL2 = 1
               DL2 = DL(IA2, ID)
            ENDIF
         ELSE IF (MYINTERP .EQ. 'A') THEN
C	     all            
            DR1 = 0.0
            DR2 = 0.0
            DL1 = 0.0
            DL2 = 0.0
            NR1 = 0
            NR2 = 0
            NL1 = 0
            NL2 = 0
            DO 492 ID = 1, NDINT
               IF (DRWT(IA1, ID) .GT. 0.0) THEN
                  NR1 = NR1 + 1
                  DR1 = DR1 + DR(IA1, ID)
               ENDIF
               IF (DRWT(IA2, ID) .GT. 0.0) THEN
                  NR2 = NR2 + 1
                  DR2 = DR2 +  DR(IA2, ID)
               ENDIF
               IF (DLWT(IA1, ID) .GT. 0.0) THEN
                  NL1 = NL1 + 1
                  DL1 = DL1 +  DL(IA1, ID)
               ENDIF
               IF (DLWT(IA2, ID) .GT. 0.0) THEN
                  NL2 = NL2 + 1
                  DL2 = DL2 +  DL(IA2, ID)
               ENDIF
 492        CONTINUE
            IF (NR1 .GT. 0) DR1 = DR1/FLOAT(NR1)
            IF (NR2 .GT. 0) DR2 = DR2/FLOAT(NR2)
            IF (NL1 .GT. 0) DL1 = DL1/FLOAT(NL1)
            IF (NL2 .GT. 0) DL2 = DL2/FLOAT(NL2)
         ELSE IF (MYINTERP .NE. '2') THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Unknown interpolation method '//INTERP)
            GOTO 990
         ENDIF
C
         IF (NR1 .GT. 0 .AND. NL2 .GT. 0 .AND.
     $       NR2 .GT. 0 .AND. NL1 .GT. 0) THEN
            RLT = RL - RR * (DR1 + CONJG(DL2))
            LRCT= LRC-RRC * (DR2 + CONJG(DL1))
            LRT = CONJG(LRCT)
            QV(IVIS) = (RLT*EP + LRT*EPC)/2.0
            UV(IVIS) = J *(LRT*EPC - RLT*EP)/2.0
         ELSE
            QW(IVIS) = 0.0
            UW(IVIS) = 0.0
         ENDIF
C
 500  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
