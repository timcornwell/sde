C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visavp1.f	1.1    6/7/93
C
      SUBROUTINE VISAVP1 (UI, UO, VI, VO, WI, WO, TIMI, TIMO,
     $   BASI, BASO, WT, TINT, NIN, NOUT, NANT)
C
CD Average visibility header items (pixel level)
C
C	UI, UO	REAL(*)	in/out	Input & output u's
C	VI, VO	REAL(*)	in/out	Input & output v's
C	WI, WO	REAL(*)	in/out	Input & output w's
C	TIMI	REAL(*)	input	Input times
C	TIMO	REAL(*)	output	Output times
C	BASI	REAL(*)	input	Input baselines
C	BASO	REAL(*)	output	Output baselines
C	WT	REAL(*)	input	Weights
C	TINT	REAL	input	Integration time
C	NIN	INT	input	Number of input items
C	NOUT	INT	input	Number of output items
C	NANT	INT	output	Maximum antenna number
C
C The assumption is made that all visibility points in a given integration
C come from the same subarray.
C
C Audit trail:
C	New routine
C				D.S.Briggs	May 20 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NIN, NOUT, NANT
      REAL		UI(NIN), UO(NOUT), VI(NIN), VO(NOUT),
     $   		WI(NIN), WO(NOUT), TIMI(NIN), TIMO(NOUT),
     $   		BASI(NIN), BASO(NOUT), WT(NIN), TINT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISAVP1')
C
      INTEGER		I, IVIS, IA1, IA2, ISTART, IEND, IOUT
      REAL		TMIN, TMAX, FSUB
C
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 40)
      REAL		UACC(MAXNANT,MAXNANT), VACC(MAXNANT,MAXNANT),
     $   		WACC(MAXNANT,MAXNANT)
      INTEGER		N(MAXNANT, MAXNANT)
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
C
C ***** Main loop over integration times *****
C
 1    CONTINUE
C
      DO 11 IA1 = 1, NANT-1
         DO 10 IA2 = IA1+1, NANT
            UACC(IA1, IA2) = 0.0
            VACC(IA1, IA2) = 0.0
            WACC(IA1, IA2) = 0.0
            N(IA1, IA2) = 0
 10      CONTINUE
 11   CONTINUE
C
C Average up all the stuff for this interval
C
      TMAX = TIMI(ISTART)
      TMIN = TIMI(ISTART)
      IEND = NIN
      DO 90 IVIS = ISTART, NIN
         IF (WT(IVIS).LE.0.0) GO TO 90
         IF (TIMI(IVIS).GT.TIMI(ISTART)+TINT) THEN
            IEND = IVIS-1
            GO TO 95
         ELSE
            TMAX = MAX(TIMI(IVIS), TMAX)
            TMIN = MIN(TIMI(IVIS), TMIN)
            IA1 = NINT(BASI(IVIS)/256.0)
            IA2 = NINT(BASI(IVIS)-FLOAT(256*IA1))
            FSUB = BASI(IVIS) - INT(BASI(IVIS))
            IF (IA1.GT.IA2) THEN
               I = IA1
               IA1 = IA2
               IA2 = I
            END IF
            N(IA1,IA2) = N(IA1,IA2) + 1
            UACC(IA1,IA2) = UACC(IA1,IA2) + UI(IVIS)
            VACC(IA1,IA2) = VACC(IA1,IA2) + VI(IVIS)
            WACC(IA1,IA2) = WACC(IA1,IA2) + WI(IVIS)
         END IF
  90  CONTINUE
  95  CONTINUE
C
      DO 110 IA1 = 1, NANT-1
         DO 100 IA2 = IA1+1, NANT
            IF (N(IA1,IA2).GT.0) THEN
               IF (IOUT.GT.NOUT) THEN
                  CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $               'Not enough output space')
                  GO TO 999
               END IF
               UO(IOUT) = UACC(IA1,IA2) / N(IA1,IA2)
               VO(IOUT) = VACC(IA1,IA2) / N(IA1,IA2)
               WO(IOUT) = WACC(IA1,IA2) / N(IA1,IA2)
               TIMO(IOUT) = (TMAX + TMIN) / 2.0
               BASO(IOUT) = 256.0 * IA1 + IA2 + FSUB
               IOUT = IOUT + 1
            END IF
 100     CONTINUE
 110  CONTINUE
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
