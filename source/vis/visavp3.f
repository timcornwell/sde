C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visavp3.f	1.1    6/7/93
C
      SUBROUTINE VISAVP3 (VISI, VISO, WTI, WTS, WTO, TIMI, BASI, TINT,
     $   SIGMAUW, NIN, NOUT, NANT, WTMODE)
C
CD Median average visibility data (pixel level)
C
C	VISI	CMPLX(*) input	Vis data
C	VISO	CMPLX(*) output	Averaged vis data
C	WTI	REAL(*)	input	Weights
C	WTS	REAL(*)	input	Selection weights from first STOKES
C	WTO	REAL	output	Averaged weights.  (Depends on WTMODE)
C	TIMI	REAL(*)	input	Input times
C	BASI	REAL(*)	input	Baslines
C	TINT	REAL(*)	input	Integration times
C	SIGMAUW	REAL	input	Sigma for unit weight
C	NIN	INT	input	Number of input visibilities
C	NOUT	INT	input	Number of output visibilities
C	NANT	INT	input	Maximum antenna number
C	WTMODE	CH*(*)	input	'WT' or 'VB-WT'
C
C Since it's not entirely obvious, the 2-D arrays are being used as:
C   N(i,j)  j>i      Number of valid samples seen on baseline i,j
C   N(i,j)  j<i      Flag to indicate that a sample has been seen on this
C                    baseline, valid or not.  (Needed to maintain
C                    correspondence between stokes parameters.)
C   WACC(i,j)  j>i   Weight sum accumulation
C   WACC(i,j)  j<i   Not used
C
C Audit trail:
C	New routine
C				D.S.Briggs	May 20 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NIN, NOUT, NANT
      COMPLEX		VISI(NIN), VISO(NOUT)
      REAL		WTI(NIN), WTS(NIN), WTO(NOUT), TIMI(NIN),
     $   		BASI(NIN), TINT, SIGMAUW
      CHARACTER*(*)	WTMODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISAVP3')
C
      INTEGER		I, IVIS, IA1, IA2, ISTART, IEND, IOUT, NS,
     $   		ISAMP, JA1, JA2
      REAL		TMIN, TMAX, MEDR, MEDI
C
      INTEGER		MAXNANT, MAXNSAMP
      PARAMETER		(MAXNANT = 40)
      PARAMETER		(MAXNSAMP = 1000)
      REAL		WACC(MAXNANT,MAXNANT),
     $   		ACCR(MAXNSAMP), ACCI(MAXNSAMP)
      INTEGER		N(MAXNANT,MAXNANT)
C
      REAL		PI
      PARAMETER		(PI=3.14159265)
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF (NANT.GT.MAXNANT) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE,
     $      'Recompile with bigger temporary antenna arrays')
         GO TO 999
      END IF
C
      IF ((WTMODE.NE.'WT').AND.(WTMODE.NE.'VB-WT')) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,'Unrecognized weight mode')
         GO TO 999
      END IF
C
      ISTART = 1
      IOUT = 1
      NS = 0
C
C ***** Main loop over integration times *****
C
 1    CONTINUE
C
      DO 11 IA2 = 1, NANT
         DO 10 IA1 = 1, NANT
            WACC(IA1,IA2) = 0.0
            N(IA1,IA2) = 0
 10      CONTINUE
 11   CONTINUE
C
C Average up all the stuff for this interval
C
      TMAX = TIMI(ISTART)
      TMIN = TIMI(ISTART)
      IEND = NIN
      DO 90 IVIS = ISTART, NIN
         IF (WTS(IVIS).LE.0.0) GO TO 90
         IF (TIMI(IVIS).GT.TIMI(ISTART)+TINT) THEN
            IEND = IVIS-1
            GO TO 95
         ELSE
            TMAX = MAX(TIMI(IVIS), TMAX)
            TMIN = MIN(TIMI(IVIS), TMIN)
            IA1 = NINT(BASI(IVIS)/256.0)
            IA2 = NINT(BASI(IVIS)-FLOAT(256*IA1))
            IF (IA1.GT.IA2) THEN
               I = IA1
               IA1 = IA2
               IA2 = I
            END IF
            N(IA2,IA1) = 1
            IF (WTI(IVIS).GT.0.0) THEN
               WACC(IA1,IA2) = WACC(IA1,IA2) + WTI(IVIS)
               N(IA1,IA2) = N(IA1,IA2) + 1
            END IF
         END IF
  90  CONTINUE
  95  CONTINUE
C
C Munge by some constants, as needed
C
      IF ((WTMODE.EQ.'VB-WT').OR.(WTMODE.EQ.'VB-RMS')) THEN
         DO 410 IA1 = 1, NANT-1
            DO 400 IA2 = IA1+1, NANT
               WACC(IA1,IA2) = WACC(IA1,IA2) * 2.0
 400        CONTINUE
 410     CONTINUE
      END IF
C
      IF ((WTMODE.EQ.'WT').OR.(WTMODE.EQ.'VB-WT')) THEN
         DO 430 IA1 = 1, NANT-1
            DO 420 IA2 = IA1+1, NANT
               WACC(IA1,IA2) = WACC(IA1,IA2) / SIGMAUW**2
 420        CONTINUE
 430     CONTINUE
      END IF
C
C Correct the weights for the larger variance of the mean, assuming
C Gaussian statistics.  See Kendall & Stuart, _The Advanced Theory of
C Statistics_, 17.12.  The limiting value for the ratio of the variances
C is .637, which would probably be good enough for most things.
C
         DO 450 IA1 = 1, NANT-1
            DO 440 IA2 = IA1+1, NANT
               I = N(IA1,IA2) / 2
               IF (I*2 .EQ. N(IA1,IA2)) THEN
                  WACC(IA1,IA2) = WACC(IA1,IA2) *
     $               (2.0/PI + (6.0/PI - 1.0)/N(IA1,IA2))
               ELSE
                  WACC(IA1,IA2) = WACC(IA1,IA2) *
     $               (2.0/PI + (4.0/PI - 1.0)/N(IA1,IA2))
               END IF
 440        CONTINUE
 450     CONTINUE
C
C Now do the bulk of the real work -- find the medians
C
      DO 520 IA1 = 1, NANT-1
         DO 510 IA2 = IA1+1, NANT
            IF (N(IA2,IA1).GT.0.0) THEN
               IF (IOUT.GT.NOUT) THEN
                  CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $               'Not enough output space')
                  GO TO 999
               END IF
               IF (N(IA1,IA2).GT.MAXNSAMP) THEN
                  CALL ERRREPOR (ERRFATAL, ROUTINE,
     $               'Increase size of MAXNSAMP in VISAVP3 & recompile')
                  GO TO 999
               END IF
C
               ISAMP = 1
               DO 500 IVIS = ISTART, IEND
                  JA1 = NINT(BASI(IVIS)/256.0)
                  JA2 = NINT(BASI(IVIS)-FLOAT(256*JA1))
                  IF ((WTI(IVIS).GT.0.0).AND.(WTS(IVIS).GT.0.0)) THEN
                     IF (JA1.GT.JA2) THEN
                        I = JA1
                        JA1 = JA2
                        JA2 = I
                     END IF
                     IF ((IA1.EQ.JA1).AND.(IA2.EQ.JA2)) THEN
                        ACCR(ISAMP) = REAL(VISI(IVIS))
                        ACCI(ISAMP) = IMAG(VISI(IVIS))
                        ISAMP = ISAMP + 1
                     END IF
                  END IF
 500           CONTINUE
C
               CALL UTLRSORT (N(IA1,IA2), ACCR, ACCR)
               CALL UTLRSORT (N(IA1,IA2), ACCI, ACCI)
               I = N(IA1,IA2) / 2
               IF (2*I.EQ.N(IA1,IA2)) THEN
                  MEDR = (ACCR(I) + ACCR(I+1)) / 2.0
                  MEDI = (ACCI(I) + ACCI(I+1)) / 2.0
               ELSE
                  MEDR = ACCR(I+1)
                  MEDI = ACCI(I+1)
               END IF
C
               WTO(IOUT) = WACC(IA1,IA2)
               VISO(IOUT) = CMPLX(MEDR,MEDI)
               IOUT = IOUT + 1
            END IF
 510     CONTINUE
 520  CONTINUE
C
C Main loopback
C
      ISTART = IEND + 1
      IF (ISTART.LE.NIN) THEN
         IF (IOUT.GT.NOUT) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Too many integrations')
            GO TO 999
         END IF
         GO TO 1
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

