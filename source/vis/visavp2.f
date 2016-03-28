C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visavp2.f	1.3    6/12/94
C
      SUBROUTINE VISAVP2 (VISI, VISO, WTI, WTS, WTO, TIMI, BASI, TINT,
     $   SIGMAUW, SSIGMA, SSIGUW, NIN, NOUT, NANT, WTMODE)
C
CD Average visibility data (pixel level)
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
C	SSIGMA	REAL	output	Measured average sample sigma
C	SSIGUW	REAL	output	Measured sigma for unit weight
C	NIN	INT	input	Number of input visibilities
C	NOUT	INT	input	Number of output visibilities
C	NANT	INT	input	Maximum antenna number
C	WTMODE	CH*(*)	input	'RMS', 'WT', 'VB-RMS' or 'VB-WT'
C
C Since it's not entirely obvious, the 2-D arrays are being used as:
C   N(i,j)  j>i      Number of valid samples seen on baseline i,j
C   N(i,j)  j<i      Flag to indicate that a sample has been seen on this
C                    baseline, valid or not.  (Needed to maintain
C                    correspondence between stokes parameters.)
C   ACC(i,j)  j>i    Visibility sum accumulation
C   ACC(i,j)  j<i    Not used
C   WACC(i,j)  j>i   Weight sum accumulation, Real RMS (Vis-Mean)^2 acc
C   WACC(i,j)  j<i   Imag RMS (Vis-Mean)^2 accumulation
C
C Audit trail:
C	New routine
C				D.S.Briggs	May 20 1993
C	Removed redundant loop in calculation of RMS's
C				D.S.Briggs	May 25 1994
C	Added estimation of variance for unit weight in RMS mode.
C	Eg. esimated SIGMAUW.
C				D.S.Briggs	May 30 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NIN, NOUT, NANT
      COMPLEX		VISI(NIN), VISO(NOUT)
      REAL		WTI(NIN), WTS(NIN), WTO(NOUT), TIMI(NIN),
     $   		BASI(NIN), TINT, SIGMAUW, SSIGMA, SSIGUW
      CHARACTER*(*)	WTMODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISAVP2')
C
      DOUBLE PRECISION	KACC
      COMPLEX		XT
      INTEGER		I, IVIS, IA1, IA2, ISTART, IEND, IOUT, NS, NK
      REAL		TMIN, TMAX, SACC, S2, S2I, S2R
C
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 42)
      COMPLEX		ACC(MAXNANT,MAXNANT)
      REAL		WACC(MAXNANT,MAXNANT)
      INTEGER		N(MAXNANT,MAXNANT)
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF (NANT.GT.MAXNANT) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE,
     $      'Recompile with bigger temporary arrays')
         GO TO 999
      END IF
C
      ISTART = 1
      IOUT = 1
      NS = 0
      SACC = 0.0
      NK = 0
      KACC = 0.D0
C
C ***** Main loop over integration times *****
C
 1    CONTINUE
C
      DO 11 IA2 = 1, NANT
         DO 10 IA1 = 1, NANT
            ACC(IA1, IA2) = (0.0,0.0)
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
               ACC(IA1,IA2) = ACC(IA1,IA2) + WTI(IVIS) * VISI(IVIS)
               N(IA1,IA2) = N(IA1,IA2) + 1
            END IF
         END IF
  90  CONTINUE
  95  CONTINUE
C
C Find means
C
      DO 110 IA1 = 1, NANT-1
         DO 100 IA2 = IA1+1, NANT
            IF (WACC(IA1,IA2).GT.0.0) THEN
               ACC(IA1,IA2) = ACC(IA1,IA2) / WACC(IA1,IA2)
            END IF
 100     CONTINUE
 110  CONTINUE
C
C Deal with weights
C
      IF ((WTMODE.EQ.'WT').OR.(WTMODE.EQ.'VB-WT')) THEN
C
C WTMODE=WT or WTMODE=VB-WT is already done, since the weights just add
C
      ELSE IF ((WTMODE.EQ.'RMS').OR.(WTMODE.EQ.'VB-RMS')) THEN
         DO 302 IA2 = 1, NANT
            DO 301 IA1 = 1, NANT
               WACC(IA1,IA2) = 0.0
 301        CONTINUE
 302        CONTINUE
            DO 300 IVIS = ISTART, IEND
               IF ((WTI(IVIS).GT.0.0).AND.(WTS(IVIS).GT.0.0)) THEN
                  IA1 = NINT(BASI(IVIS)/256.0)
                  IA2 = NINT(BASI(IVIS)-FLOAT(256*IA1))
                  IF (IA1.GT.IA2) THEN
                     I = IA1
                     IA1 = IA2
                     IA2 = I
                  END IF
                  XT = VISI(IVIS)-ACC(IA1,IA2)
                  WACC(IA1, IA2) = WACC(IA1,IA2) + ABS(XT)**2
                  IF (N(IA1,IA2).GT.2) THEN
                     KACC = KACC + (ABS(XT)**2 * 2 * WTI(IVIS)
     $                  * N(IA1,IA2) / REAL(N(IA1,IA2)-1))
                     NK = NK + 2
                  END IF
               END IF
 300        CONTINUE
         IF (SYSDEBUG) THEN
            DO 310 IA1 = 1, NANT-1
               DO 305 IA2 = IA1+1, NANT
                  IF (N(IA1,IA2).GT.2) THEN
                     WRITE (MESSAGE, 1300) IA1, IA2, N(IA1,IA2)
 1300                FORMAT(I3,',',I3,': ',I6)
                     CALL MSGPUT (MESSAGE, 'D')
                  END IF
 305           CONTINUE
 310        CONTINUE
         END IF
         DO 340 IA1 = 1, NANT-1
            DO 330 IA2 = IA1+1, NANT
               IF (N(IA1,IA2).GE.2) THEN
                  S2R = WACC(IA1,IA2) / (N(IA1,IA2)-1)
                  S2I = WACC(IA2,IA1) / (N(IA1,IA2)-1)
                  S2 = (S2R + S2I) / 2.0
                  WACC(IA1,IA2) = N(IA1,IA2) / S2
                  SACC = SACC + SQRT(S2)
c                  print *, SQRT(S2), SQRT(S2R), SQRT(S2I)
                  NS = NS + 1
               ELSE
                  WACC(IA1,IA2) = 0.0
               END IF
 330        CONTINUE
 340     CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,'Unrecognized weight mode')
         GO TO 999
      END IF
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
      DO 510 IA1 = 1, NANT-1
         DO 500 IA2 = IA1+1, NANT
            IF (N(IA2,IA1).GT.0.0) THEN
               IF (IOUT.GT.NOUT) THEN
                  CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $               'Not enough output space')
                  GO TO 999
               END IF
               WTO(IOUT) = WACC(IA1,IA2)
               VISO(IOUT) = ACC(IA1,IA2)
               IOUT = IOUT + 1
            END IF
 500     CONTINUE
 510  CONTINUE
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
      SSIGMA = SACC / NS
      SSIGUW = SQRT(KACC/NK) / SQRT(2.0)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

