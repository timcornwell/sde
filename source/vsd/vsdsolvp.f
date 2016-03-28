C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdsolvp.f	1.1	 5/4/93
C
      SUBROUTINE VSDSOLVP (
     $   IV, IM, IW, QV, QM, QW, UV, UM, UW, 
     $   BASL, VTIME, ROTATED,
     $   NVIS, NANT, NUMDINT, NUMVINT, TINTD, 
     $   DRL, DTIME, DRWT, DLWT,
     $   IWORK1, XWORK1, RWORK2,
     $   DATEOBS, OBSRA, OBSDEC, EPOCH, SLAT, SLON,
     $   REFANT)
C
CD From I, Q, U visibilities and I, Q, U models, determine Dr and Dl
C
C	IV	X(*)		inp	Input I visibilities
C	IM	X(*)		inp	I Model visibilities
C	IW	R(*)		inp	Input I weights
C					same for Q, U
C	BASL	REAL(*)		input	Baselines
C	VTIME	REAL(*)		input	Times, in days
C	ROTATED	L		input	Paralactic angle corrections?
C	NVIS	INT		input	Number of visibilities
C	NANT	INT		input	Number of antennas
C	NUMDINT	INT		input	Number of intervals for D terms
C	NUMVINT	INT		input	Number of intervals for visibilities
C	TINTD	REAL		input	Time interval for D terms, days
C	DRL	X(2*NANT,NUMDINT) out	DR, DL*
C	DTIME	REAL(*)		output	Time at which DR, DL are valid
C	DRWT	REAL(NANT,NUMDI)  out	Wt: 1 for valid, 0 or less for invalid      
C	DLWT	REAL(NANT,NUMDI)  out	Wt: 1 for valid, 0 or less for invalid      
C	DRLRMS	R(2*NANT,NUMDINT) out	Estimate of error in DR, DL
C	ADRLRMS	R		output	Average error in DR, DL
C	IWORK1	I(NANT)			scratch
C	XWORK1	X(2*NANT)		scratch
C	RWORK2	R(2*NANT, 2*NANT)	scratch
C       DATEOBS CH*(*)          in      '23/01/91'
C       OBSDEC  DBLE            in      Ibserved Dec, degrees
C       OBSRA   DBLE            in      Observed RA, degrees
C       EPOCH   REAL            in      Epoch of RA, DEC
C       SLAT    REAL            in      Site Latitude, degrees
C       SLON    REAL            in      Site Longitude, degrees
C	REFANT	INT		in	Referance antenna
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Sept 5 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NANT, NUMDINT, NUMVINT
      COMPLEX		IV(*), UV(*), QV(*)
      COMPLEX		IM(*), UM(*), QM(*)
      REAL		IW(*), UW(*), QW(*)
      REAL		BASL(*), VTIME(*)
      COMPLEX		DRL(2*NANT,*)
      REAL		DTIME(*), TINTD, DRWT(NANT,*), DLWT(NANT,*)
      INTEGER		IWORK1(NANT)
      COMPLEX		XWORK1(2*NANT)
      REAL		RWORK2(2*NANT, 2*NANT)
      CHARACTER*(*)     DATEOBS
      REAL              EPOCH, SLAT, SLON
      DOUBLE PRECISION  OBSRA, OBSDEC
      LOGICAL		ROTATED
      INTEGER		REFANT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDSOLVP')
C
      REAL		XWORKR(100), XWORKI(100), DRREAL(100), DRIMAG(100)
      COMPLEX		J, RLT, LRT, LRCT
      COMPLEX		RR, RL, RRM, RLM
      COMPLEX		LRC, LRMC, RRC, RRMC
      COMPLEX		EMC, EPC, EM, EP
      COMPLEX		DRREF
      REAL		PA1, PA2, D2R, TINTV
      INTEGER		IVIS, IA1, IA2, INTNDX
      REAL		PI, SUM
      PARAMETER		(PI=3.14159274101257)
      INTEGER		ISTART, IEND, NGOOD, NW, IX, IY
      DOUBLE PRECISION  RANOW, DECNOW, MJD, SLAEPB, DATE, LST, SLAGMST,
     $                  SLATR, DECNOWR, HAR, AZ, EL, PA
C
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
C	DRL			has DR from 1-NANT, has DL* from 
C				NANT+1 to 2*NANT
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
      INTNDX = 1
      ISTART = 1
      IEND = 1
      CALL PIXMDIFF (VTIME, NVIS, TINTV)
      IF (TINTV .GT. 1.0) TINTV = 10.0/3600.0/24.0
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
  1   CONTINUE
C
C Loop over data for this integration
C
      NGOOD = 0
      DTIME(INTNDX) = 0.0
      DO 100 IVIS = ISTART, NVIS
         IF (IW(IVIS).LE.0.0) GO TO 100
         IF (DTIME(INTNDX) .EQ. 0.0) THEN
            DTIME(INTNDX) = VTIME(IVIS) + TINTD/2.0
         ENDIF
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
      IF (NGOOD.EQ.0) GO TO 560
C
C Form solution
C
      CALL PIXXSETC (XWORK1, 0.0, 2*NANT)
      CALL PIXISETC (IWORK1, 0.0, NANT)
      CALL PIXRSETC (RWORK2, 0.0, 4*NANT*NANT)
      DO 290 IA1 = 1, NANT
         RWORK2(IA1, IA1) = 1.0
         RWORK2(NANT+IA1, NANT+IA1) = 1.0
 290  CONTINUE
C
C Work on WORK1 and WORK2 for general case: 
C  (we do not yet support VLBI, or where PA1 .NE. PA2)
C
      DO 300 IVIS = ISTART, IEND
         IF (IW(IVIS).LE.0.0) THEN
            GO TO 300
         END IF
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
C
C We have an indexing problem somewhere, and antennas
C 1 and 2 corrupt the solution.  Quick Fix
C
         IF (IA1 .EQ. 1  .OR. IA2 .EQ. 1  .OR.
     $      IA1 .EQ. 2   .OR. IA2 .EQ. 2  
C     $      .OR. IA1 .EQ. 27   .OR. IA2 .EQ. 27
C     $      .OR. IA1 .EQ. 28   .OR. IA2 .EQ. 28
     $      ) THEN
            IW(IVIS) = -ABS( IW(IVIS) )
            QW(IVIS) = -ABS( QW(IVIS) )
            UW(IVIS) = -ABS( UW(IVIS) )
            GOTO 300
         ENDIF            
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
C Note: RR, LL, RL, LRC here represent data visibilities,
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
C RRM, etc, represent MODEL visibilities, unrotated.
C The rotation goes on the forming XWORK1, RWORK2.
C
         RRM= IM(IVIS)
         RRMC = CONJG ( RRM )
         RLM= QM(IVIS) + J * UM(IVIS) 
         LRMC = CONJG (QM(IVIS) - J * UM(IVIS)  )
C
         IWORK1(IA1) =  IWORK1(IA1) + 1
         IWORK1(IA2) =  IWORK1(IA2) + 1
C
C D_{R} Terms
C
         XWORK1(IA1) = XWORK1(IA1)  + 
     $      (RL - RLM*EPC)/ ( RR * EM )
         XWORK1(IA2) = XWORK1(IA2) +
     $      (LRC - LRMC*EPC) / (RRC * EMC )
         RWORK2(IA1, NANT+IA2) = RWORK2(IA1, NANT+IA2) + EMC*EMC
         RWORK2(IA2, NANT+IA1) = RWORK2(IA2, NANT+IA1) + EM*EM
C
C D_{L}^{*} Terms
C
         XWORK1(IA1 + NANT) = XWORK1(IA1 + NANT) +
     $      (LRC - LRMC*EPC) / (RRC * EM )
         XWORK1(IA2 + NANT) = XWORK1(IA2 + NANT)  +
     $      (RL - RLM*EPC)/ ( RR * EMC )
         RWORK2(NANT+IA1, IA2) = RWORK2(NANT+IA1, IA2) + EMC*EMC
         RWORK2(NANT+IA2, IA1) = RWORK2(NANT+IA2, IA1) + EM*EM
         SUM = -NANT
         DO 2727 IY = 1, NANT
            DO 2726 IX = 1, NANT
               SUM = SUM + RWORK2(IX, IY)
 2726       CONTINUE
 2727    CONTINUE
         IF (SUM .NE. 0.0) THEN
            WRITE (MESSAGE, 2728) SUM, IA1, IA2, IVIS
 2728       FORMAT ('Trouble with SUM: ',F10.2,3I10)
         ENDIF
 300  CONTINUE
C
      DO 340 IA1 = 1, NANT
         IF (IWORK1(IA1) .EQ. 0) THEN
            IF (SYSDEBUG) THEN
               WRITE (MESSAGE, 9192) IA1, INTNDX
 9192          FORMAT ('Antenna ',I3,' has no vis in int ',I3)
               CALL MSGPUT (MESSAGE, 'W')
            ENDIF
            XWORK1(IA1) = CMPLX ( 0., 0.)
            XWORK1(NANT+IA1) = CMPLX ( 0., 0.)
            DO 325 IA2 = 1, NANT
               RWORK2(IA1, NANT+IA2) = 0.0
               RWORK2(NANT+IA1, IA2) = 0.0
 325        CONTINUE
         ELSE
            DRWT(IA1, INTNDX) = 1.0
            DLWT(IA1, INTNDX) = 1.0
            XWORK1(IA1) = XWORK1(IA1) / FLOAT(IWORK1(IA1) )
            XWORK1(NANT+IA1) = XWORK1(NANT+IA1)/ FLOAT(IWORK1(IA1) )
            DO 330 IA2 = 1, NANT
               RWORK2(IA1, NANT+IA2) = RWORK2(IA1, NANT+IA2) /
     $            FLOAT(IWORK1(IA1) )
               RWORK2(NANT+IA1, IA2) = RWORK2(NANT+IA1, IA2) /
     $            FLOAT(IWORK1(IA1) )
 330        CONTINUE
         ENDIF
 340  CONTINUE
C
      IF (SYSDEBUG) THEN
         IF (NANT .GT. 16) THEN
            NW = 16
            CALL MSGPUT ('Matrix too big to write, only 16 collumns',
     $         'W')
         ELSE
            NW = NANT
         ENDIF
         CALL MSGPUT ('Input Matrix:', 'I')
         DO 402 IA1 = 1, 2*NANT
            WRITE (MESSAGE, 1919) (RWORK2(IA1,IA2), IA2=1, 2*NW)
 1919       FORMAT (<2*NW>(1X,F6.3, 1X))
            CALL MSGPUT (MESSAGE, 'I')
 402     CONTINUE
         CALL MSGPUT ('XWORK2:', 'X')
         DO 403 IA1 = 1, 2*NANT
            WRITE (MESSAGE, 1920) IA1, XWORK1(IA1)
 1920       FORMAT (I4,2F10.6)
            CALL MSGPUT (MESSAGE, 'I')
 403     CONTINUE
      ENDIF
C
      DO 400 IA1 = 1, 2*NANT
         XWORKR(IA1) = REAL(XWORK1(IA1))
         XWORKI(IA1) = AIMAG(XWORK1(IA1))
 400  CONTINUE
      CALL SVDSOLV (RWORK2, DRREAL, XWORKR, 2*NANT, 2*NANT)
      CALL SVDSOLV (RWORK2, DRIMAG, XWORKI, 2*NANT, 2*NANT)
C
      DRREF = CMPLX (DRREAL(REFANT), DRIMAG(REFANT) )
      DO 410 IA1 = 1, NANT
         DRL(IA1, INTNDX) = CMPLX ( DRREAL(IA1), DRIMAG(IA1) )
     $      - DRREF
         DRL(NANT+IA1, INTNDX) = CMPLX ( DRREAL(NANT+IA1),
     $      DRIMAG(NANT+IA1) ) 
     $      + DRREF
 410  CONTINUE
C
C Insert corrected values for Q, U
C
      DO 500 IVIS = ISTART, IEND
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
C Note: RR, LL, RL, LRC here represent data visibilities,
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
         RRM= IM(IVIS)
         RRMC= CONJG(RRM)
         RLT = RL - RR * (DRL(IA1, INTNDX) + DRL(NANT+IA2, INTNDX))
         LRCT = LRC-RRC* (DRL(IA2, INTNDX) + DRL(NANT+IA1, INTNDX))
C
         LRT = CONJG(LRCT)
         QV(IVIS) = (RLT*EP + LRT*EPC)/2.0
         UV(IVIS) = J *(LRT*EPC - RLT*EP)/2.0
C
 500  CONTINUE
 560  CONTINUE
C
C Now move onto the next integration interval
C
      WRITE (MESSAGE, 9836) INTNDX, PA1, PA2
 9836 FORMAT ('Integration: ',I5, '  PA1= ',F10.3,
     $   ' PA2= ',F10.3)
      CALL MSGPUT (MESSAGE, 'D')
      ISTART = IEND + 1
      INTNDX = INTNDX + 1
C
C Any data left?
C
      IF (ISTART.LE.NVIS) THEN
         IF (INTNDX.GT.NUMDINT) THEN
C            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Too many integrations')
C            GO TO 999
            CALL MSGPUT ('Too many integrations', 'W')
            DO 800 IVIS = ISTART, NVIS
               IW(IVIS) = 0.0
               QW(IVIS) = 0.0
               UW(IVIS) = 0.0
 800        CONTINUE
            GOTO 980
         END IF
         GO TO 1
      END IF
C
C ********************** End of loop over integrations *****************
C
 980  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
