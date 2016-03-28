C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vispavep.f	1.4	 2/24/95
C
      SUBROUTINE VISPAVEP (IV, IW, QV, QW, UV, UW, VV, VW,
     $   BASL, VTIME, ROTATE, NVIS, NANT, 
     $   DATEOBS, OBSRA, OBSDEC, EPOCH, SLAT, SLON, DIVI, TXTFILE)
C
CD Find average value of (RL + LR*)/2, pixel level
C
C	QV	X(*)		in	Input Q visibilities
C	QW	R(*)		in	Input Q weights
C	UV	X(*)		in	Input U visibilities
C	UW	R(*)		in	Input U weights
C	BASL	REAL(*)		input	Baselines
C	VTIME	REAL(*)		input	Times, in days
C	ROTATE	I		input	Paralactic angle corrections:
C					    -1=undo | 0 = nothing | 1=do
C	NVIS	INT		input	Number of visibilities
C	NANT	INT		input	Number of antennas
C       DATEOBS CH*(*)          in      '23/01/91'
C       OBSDEC  DBLE            in      Observed Dec, degrees
C       OBSRA   DBLE            in      Observed RA, degrees
C       EPOCH   REAL            in      Epoch of RA, DEC
C       SLAT    REAL            in      Site Latitude, degrees
C       SLON    REAL            in      Site Longitude, degrees
C	TXTFILE	CH(*)		in	Output text file
C	DIVI	L		in	Divide by mean I?
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	April 22 1993
C	Changed ROTATE to DO UNDO NOTHING
C				M.A.Holdaway	May 12 1993
C       Changed J = CMPLX(0,1.0) to J = CMPLX(0.0,1.0) for DEC Alpha
C       F77.
C                               J.D.Ellithorpe  Oct 20 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NANT, ROTATE
      COMPLEX		UV(*), QV(*), IV(*), VV(*)
      REAL		UW(*), QW(*), IW(*), VW(*)
      REAL		BASL(*), VTIME(*)
      CHARACTER*(*)     DATEOBS, TXTFILE
      REAL              EPOCH, SLAT, SLON
      DOUBLE PRECISION  OBSRA, OBSDEC
      LOGICAL		DIVI
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPAVEP')
C
      COMPLEX		J, ALLAVE
      COMPLEX		RL, LRC
      COMPLEX		EMC, EPC, EM, EP
      REAL		PA1, PA2, D2R, RRAMP, LLAMP
      INTEGER		IVIS, IA1, IA2, NALL
      REAL		PI
      PARAMETER		(PI=3.14159274101257)
      COMPLEX		RLAVE, RRAVE, LLAVE
      REAL		REALAVE, IMAGAVE, AMP, THETA, TIME
      INTEGER		NAVE, NRRAVE, NLLAVE
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
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL TXTOPEN (ROUTINE, TXTFILE, 'WRITE')
      IF (DIVI) THEN
         CALL TXTWRITE (ROUTINE, ' Average (RL + LR*)/2RR ')
      ELSE
         CALL TXTWRITE (ROUTINE, ' Average (RL + LR*)/2 ') 
      ENDIF
      CALL TXTWRITE (ROUTINE, ' Time    RL AMP    RL PHASE     '//
     $   'RR AMP    LL AMP')
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
      RRAVE = 0.0
      LLAVE = 0.0
      RLAVE = 0.0
      TIME = VTIME(1)
      NAVE = 0
      NALL = 0
      ALLAVE = 0.0
      NRRAVE = 0
      NLLAVE = 0
      DO 500 IVIS = 1, NVIS
         IF (QW(IVIS).LE.0.0 .OR. UW(IVIS) .LE.0.0) THEN
            GO TO 500
         END IF
         IF (IW(IVIS).LE.0.0) GOTO 500
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
C The requirement is OPPOSITE what is used in VISDSOLP:
C
         IF (ROTATE .EQ. 0) THEN
            RL = QV(IVIS) + J * UV(IVIS)
            LRC = CONJG  (QV(IVIS) - J * UV(IVIS) )
         ELSE IF (ROTATE .GT. 0) THEN
            RL = (QV(IVIS) + J * UV(IVIS) ) * EP
            LRC = CONJG ( (QV(IVIS) - J * UV(IVIS) ) * EPC )
         ELSE IF (ROTATE .LT. 0) THEN
            RL = (QV(IVIS) + J * UV(IVIS) ) * EPC
            LRC = CONJG ( (QV(IVIS) - J * UV(IVIS) ) * EP )
         ENDIF
C
         IF (VTIME(IVIS) .NE. TIME) THEN
            IF (NAVE .GT. 0) THEN
               RLAVE = RLAVE / FLOAT(NAVE)
               IF (DIVI .AND. NRRAVE .GT. 0) THEN
                  RRAVE = RRAVE / FLOAT(NRRAVE)
                  LLAVE = LLAVE / FLOAT(NLLAVE)
                  RLAVE = RLAVE / CABS(RRAVE)
               ENDIF
               AMP = CABS(RLAVE)
               REALAVE = REAL(RLAVE)
               IMAGAVE = AIMAG(RLAVE)
               THETA = ATAN2(IMAGAVE, REALAVE) / D2R
               RRAMP = CABS(RRAVE)
               LLAMP = CABS(LLAVE)
               WRITE (MESSAGE, 1923) TIME, AMP, THETA, RRAMP, LLAMP
 1923          FORMAT (F10.7, F10.6, F10.3, 2F10.5)
               CALL TXTWRITE (ROUTINE, MESSAGE)
               ALLAVE = ALLAVE + RLAVE
               NALL = NALL + 1
            ENDIF
            TIME = VTIME(IVIS)
            RLAVE = 0.0
            RRAVE = 0.0
            NRRAVE = 0.0
            LLAVE = 0.0
            NLLAVE = 0.0
            NAVE = 0.0
         ENDIF
         NAVE = NAVE + 1
         RLAVE = RLAVE + (RL + LRC)/2.0
         RRAVE = RRAVE + IV(IVIS) + VV(IVIS)
         NRRAVE = NRRAVE + 1
         LLAVE = LLAVE + IV(IVIS) - VV(IVIS)
         NLLAVE = NLLAVE + 1
 500  CONTINUE
C
      CALL TXTWRITE (ROUTINE, ' ')
      ALLAVE = ALLAVE/ FLOAT(NALL)
      AMP = CABS(ALLAVE)
      REALAVE = REAL(ALLAVE)
      IMAGAVE = AIMAG(ALLAVE)
      THETA = ATAN2(IMAGAVE, REALAVE) / D2R
      WRITE (MESSAGE, 1924) AMP, THETA
 1924 FORMAT ('Global Ave: ', F10.6, F10.3)
      CALL TXTWRITE (ROUTINE, MESSAGE)
C
      CALL MSGPUT ('RL averages in file '//TXTFILE, 'I')
      CALL TXTCLOSE (ROUTINE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
