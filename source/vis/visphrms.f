C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visphrms.f	1.5	 11/15/91
C
      SUBROUTINE VISPHRMS (TIME, BL, UU, VV, VIS, NVIS, ATIMES, NT, 
     $   RMS)
C
#define nv 8192
C
CD Calculates RMS phase over various averaging times
C
C	TIME	REAL(*)	input	time of visibility measurement
C	BL	REAL(*)	input	Encoded Baseline
C	UU	REAL(*)	input	U value of measurement
C	VV	REAL(*)	input	V value of measurement
C	VIS	X(*)	input	complex visibility
C	NVIS	INT	input	number of visibilities
C	ATIMES	REAL(*)	input	averaging times
C	NT	INT	input	number of averaging times
C	RMS	CH*(*)	input	Name of RMS info directory
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	July 8 1991
C	Currently disabled all but AVE options
C				M.A.Holdaway	Aug 7 1991
C	Removed a "!" inline comment
C				M.A.Holdaway	Sep 13 1991
C	Fixed bug: bomed when an antenna number is missing
C	(ie, real data)
C				M.A.Holdaway	Nov 15 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NVIS, NT
      REAL		UU(*), VV(*), TIME(*), ATIMES(*), BL(*)
      COMPLEX		VIS(*)
      CHARACTER*(*)	RMS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPHRMS')
C
      INTEGER		IA1, IA2, JA1, JA2, NANT
      INTEGER		SUVADD, RUVADD, SMADD, RMADD, STADD, RTADD
      INTEGER		I, NBL, NV, IBL, IV, IVIS, NAXIS(SYSMXDIM)
      INTEGER		JBL, KBL, IT, DONE(2000)
      INTEGER		IND, JND, IUVMIN, JJBL
      REAL		BTIME(nv), BU(nv), BV(nv), BPH(nv)
      REAL		R2D, VI, VR
      REAL		UVMIN, PHAVE, UVAVE
      REAL		BASES(2000)
      LOGICAL		SOME
C
C=======================================================================
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER			UTLGINT
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      R2D  = 180./ ( 4.0 * ATAN2( 1.0, 1.0 ) )
      NANT = 0
      DO 50  I = 1, 2000
         BASES(I) = 0.0
 50   CONTINUE
C
      KBL = 0
      DO 100 I = 1, NVIS
         TIME(I) = TIME(I) * 24.0 * 3600.0
         IA1 = NINT (BL(I)/256.0)
         IA2 = NINT (BL(I) - FLOAT(256 * IA1))
         NANT = MAX (NANT, IA1)
         NANT = MAX (NANT, IA2)
         DO 80 IBL = 1, KBL
            IF (BL(I) .EQ. BASES(IBL)) GOTO 100
 80      CONTINUE
         KBL = KBL + 1
         BASES(KBL) = BL(I)
 100  CONTINUE
      NBL = KBL
      IF (NANT .EQ. 0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No Antennas!')
         GOTO 990
      ENDIF
      WRITE (MESSAGE, 1017) NBL
 1017 FORMAT ('Found ',I4,' baselines')
      CALL MSGPUT (MESSAGE, 'I')
C
      CALL DATMAKAR (STRM2(RMS, 'SCRATCH/UV'), 1, NBL, 'R', SUVADD)
      CALL DATMAKAR (STRM2(RMS, 'SCRATCH/TIME'), 1, NT, 'R', STADD)
      CALL DATMAKAR (STRM2(RMS, 'UV'), 1, NBL, 'R', RUVADD)
      CALL DATMAKAR (STRM2(RMS, 'TIME'), 1, NT, 'R', RTADD)
      NAXIS(1) = NT
      NAXIS(2) = NBL
      CALL DATMAKAR (STRM2(RMS, 'SCRATCH/MAT'), 2, NAXIS, 'R', SMADD)
      CALL DATMAKAR (STRM2(RMS, 'MAT'), 2, NAXIS, 'R', RMADD)
      CALL ARRSETCO (STRM2(RMS, 'MAT'), 0.0, 0.0)
      CALL ARRSETCO (STRM2(RMS, 'SCRATCH/MAT'), 0.0, 0.0)
      CALL ARRSETCO (STRM2(RMS, 'UV'), 0.0, 0.0)
      CALL ARRSETCO (STRM2(RMS, 'TIME'), 0.0, 0.0)
      CALL ARRSETCO (STRM2(RMS, 'SCRATCH/UV'), 0.0, 0.0)
      CALL ARRSETCO (STRM2(RMS, 'SCRATCH/TIME'), 0.0, 0.0)
C
      DO 110 I = 1, NT
         MEMR(STADD + I - 1) = ATIMES(I)
         MEMR(RTADD + I - 1) = ATIMES(I)
 110  CONTINUE
C
      IBL = 0
      DO 900 IA1 = 1, NANT-1
         DO 800 IA2 = IA1 + 1, NANT
            SOME = .FALSE.
C
C Isolate the IA1-IA2 baseline data
C
            IV = 0
            DO 400 IVIS = 1, NVIS
               JA1 = NINT (BL(IVIS)/256.0)
               JA2 = NINT (BL(IVIS) - FLOAT(256 * IA1))
               IF (JA1 .EQ. IA1 .AND. JA2 .EQ. IA2) THEN
                  SOME = .TRUE.
                  IV = IV + 1
                  IF (IV .GT. nv) THEN
                     CALL ERRREPOR (ERRBDARG, ROUTINE,
     $                  'Need to increase nv')
                     GOTO 990
                  ENDIF
                  BTIME(IV) = TIME(IVIS) - TIME(1)
                  BU(IV)    = UU(IVIS)
                  BV(IV)    = VV(IVIS)
                  VR        = REAL (VIS(IVIS))
                  VI        = AIMAG (VIS(IVIS))
                  IF (VI .EQ. 0. .AND. VR .EQ.  0.) THEN
                     BPH(IV) = 0.
                  ELSE
                     BPH(IV)   = R2D * ATAN2 (VI, VR)
                  ENDIF
                  IF (BPH(IV) .LT. 0.0) BPH(IV) = BPH(IV) + 360.
               ELSE IF (JA1 .EQ. IA2 .AND. JA2 .EQ. IA1) THEN
                  SOME = .TRUE.
                  IV = IV + 1
                  IF (IV .GT. nv) THEN
                     CALL ERRREPOR (ERRBDARG, ROUTINE,
     $                  'Need to increase nv')
                     GOTO 990
                  ENDIF
                  BTIME(IV) = TIME(IVIS) - TIME(1)
                  BU(IV)    = UU(IVIS)
                  BV(IV)    = VV(IVIS)
                  VR        = REAL (VIS(IVIS))
                  VI        = AIMAG (VIS(IVIS))
                  IF (VI .EQ. 0. .AND. VR .EQ.  0.) THEN
                     BPH(IV) = 0.
                  ELSE
                     BPH(IV)   = - R2D * ATAN2 (VI, VR)
                  ENDIF
                  IF (BPH(IV) .LT. 0.0) BPH(IV) = BPH(IV) + 360.
               ENDIF
 400        CONTINUE
            NV = IV
C
C Make the phases "continuous" across the 2.pi ambiguity
C
            IF (SOME) THEN
               IBL = IBL + 1
               DO 450 IV = 2, NV
                  IF (ABS(BPH(IV)  - BPH(IV-1)) .GT.  180.) THEN
                     BPH(IV) = BPH(IV) + 360.0 * 
     $                  UTLGINT ( (BPH(IV-1)  - BPH(IV) + 180.0)/360.)
                  ENDIF
 450           CONTINUE
C
C Now call the real workhorse
C
               DO 500 IT = 1, NT
                  CALL VISPHRMP (BTIME, BU, BV, BPH, NV, 
     $               MEMR(RTADD+IT-1), UVAVE, PHAVE)
                  IND = (IBL - 1) * NT + IT - 1
                  MEMR(SMADD + IND) = PHAVE
 500           CONTINUE
               MEMR (SUVADD + IBL - 1) = UVAVE
            ENDIF
 800     CONTINUE
 900  CONTINUE
C
C Sort out SCRATCH and put it into the proper locations
C
      JJBL = 0
      DO 1100 JBL = 1, NBL
C
C Get smallest UV baseline yet undone
C
         UVMIN = 9.E+30
         IUVMIN = 0
         DO 1050 IBL = 1, NBL
            DO 1010 KBL = 1, NBL
               IF (IBL .EQ. DONE(KBL)) GOTO 1050
 1010       CONTINUE
            IF (MEMR(SUVADD + IBL - 1) .LT. UVMIN ) THEN
               UVMIN = MEMR(SUVADD + IBL - 1)
               IUVMIN = IBL
            ENDIF
 1050    CONTINUE
C
C If we really have a baseline, load it into ordered phase matrix
C
         IF (IUVMIN .NE. 0) THEN
            JJBL = JJBL + 1
            DONE(JJBL) = IUVMIN
            MEMR(RUVADD + JJBL - 1) = MEMR(SUVADD + IUVMIN - 1)
            DO 1060 IT = 1, NT
               JND = (JJBL - 1) * NT + IT - 1
               IND = (IUVMIN - 1) * NT + IT - 1
               MEMR(RMADD + JND) = MEMR(SMADD + IND)
 1060       CONTINUE
         ENDIF
 1100 CONTINUE
C
      CALL DATDELET (STRM2(RMS, 'SCRATCH'))
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
