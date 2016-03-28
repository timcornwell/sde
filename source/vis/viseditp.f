C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)viseditp.f	1.3    5/15/92
C
      SUBROUTINE VISEDITP (VIS, MVIS, BASL, TIME, WT, MWT, NVIS, 
     1   NANT, TINT, MODE, NEWVIS, NEWWT, NUMINT, THRES, 
     2   ORES, NRES, NFLAG, ORESID, NRESID)
C
CD Edit data on basis of local rms. Returns global
C residuals and stores residuals for individual integrations.
C
C     2   ORES, NRES, NFLAG, ORESID, NRESID)
C
C	VIS	CMPLX	input	Input visibilities
C	MVIS	CMPLX	input	Input model visibilities
C	BASL	REAL(*)	input	Baselines
C	TIME	REAL(*)	input	Times
C	WT	REAL(*)	input	Input weights
C	MWT	REAL(*)	input	Input model weights
C	NVIS	INT	input	Number of visibilities
C	NANT	INT	input	Number of antennas
C	TINT	REAL	input	Integration time
C	MODE	CH*(*)	input	' ' for phase only |'AMPPHI' for both
C	NEWVIS	CMPLX	output	Output visibilities
C	NEWWT	REAL	output	Output weights
C	NUMINT	INT	input	Number of integrations
C      THRES   REAL    input   Threshold for flagging
C	ORES	REAL	output	Original residuals for each int.
C	NRES	REAL	output	Final residuals for each int.
C	NFLAG	INT	output	Number of vis. flagged
C	ORESID	REAL	output	Residual before correction
C	NRESID	REAL	output	Residual after correction
C Audit trail:
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NANT, NUMINT, NFLAG
      REAL		BASL(*), TIME(*), WT(*), MWT(*)
      REAL		NEWWT(*), ORESID, NRESID, TINT
      REAL		ORES(*), NRES(*), THRES
      COMPLEX		VIS(*), MVIS(*), NEWVIS(*)
      CHARACTER*(*)	MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISEDITP')
C
      CHARACTER*12	STRTIMC
      INTEGER		IVIS, NCOR, IA1, IA2, INTNDX
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 28)
      COMPLEX		CVIS(MAXNANT,MAXNANT)
      REAL		CWT(MAXNANT,MAXNANT)
      INTEGER		NSUM(MAXNANT,MAXNANT)
      REAL		IRESID, SCANWT, TOTALWT, TMAX, TMIN
      INTEGER		ISTART, IEND, NGOOD
      LOGICAL		STRMATCH
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C ********************** Start of loop over integrations *************
C
      INTNDX = 1
      ISTART = 1
      IEND = 1
C
      TOTALWT = 0.0
      NFLAG = 0
C
  1   CONTINUE
C
C Initialize accumulators
C
      DO 10 IA2 = 1, NANT
         DO 11 IA1 = 1, NANT
            CVIS(IA1, IA2) = 0.0
            CWT(IA1, IA2) = 0.0
            NSUM(IA1, IA2) = 0
  11     CONTINUE
  10  CONTINUE
C
C Find nominal time for gain
C
      TMAX = TIME(ISTART)
      TMIN = TIME(ISTART)
      NGOOD = 0
      IEND = NVIS
      DO 90 IVIS = ISTART, NVIS
         IF ((WT(IVIS).LE.0.0).OR.(MWT(IVIS).LE.0.0).OR.
     1      (ABS(MVIS(IVIS)).EQ.0.0)) GO TO 90
         IF (TIME(IVIS).GT.TIME(ISTART)+TINT) THEN
            IEND = IVIS-1
            GO TO 95
         ELSE
            TMAX = MAX(TIME(IVIS), TMAX)
            TMIN = MIN(TIME(IVIS), TMIN)
            NGOOD = NGOOD + 1
         END IF
  90  CONTINUE
  95  CONTINUE
C
C
C Loop over data for this integration
C
      DO 100 IVIS = ISTART, IEND
         IF ((WT(IVIS).LE.0.0).OR.(MWT(IVIS).LE.0.0)) GO TO 100
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
         CVIS(IA1, IA2) = CVIS(IA1, IA2) + VIS(IVIS) - MVIS(IVIS)
         CVIS(IA2, IA1) = CVIS(IA2, IA1) + CONJG(VIS(IVIS) - MVIS(IVIS))
         CWT(IA1, IA2) = CWT(IA1, IA2) + SQRT(MWT(IVIS) * WT(IVIS))
         CWT(IA2, IA1) = CWT(IA2, IA1) + SQRT(MWT(IVIS) * WT(IVIS))
         NSUM(IA1, IA2) = NSUM(IA1, IA2) + 1
         NSUM(IA2, IA1) = NSUM(IA2, IA1) + 1
 100  CONTINUE
C
C Normalize correctly
C
      DO 200 IA2 = 1, NANT
         DO 210 IA1= 1, NANT
            IF (NSUM(IA1, IA2).GT.0) THEN
               CVIS(IA1, IA2) = CVIS(IA1, IA2) / NSUM(IA1, IA2)
               CWT(IA1, IA2) = CWT(IA1, IA2) / NSUM(IA1, IA2)
            END IF
 210     CONTINUE
 200  CONTINUE
C
C Find editting for this set of data
C
      CALL VISEDITS (CVIS, CWT, NANT, THRES, MODE, ORES(INTNDX),
     $   NRES(INTNDX), SCANWT)
      IF (ERROR) THEN
         CALL ERRREASO (MESSAGE)
         IF (STRMATCH(MESSAGE,ERRNTFND)) THEN
            CALL ERRCANCE
         ELSE
            GO TO 990
         END IF
      ELSE
         TOTALWT = TOTALWT + SCANWT
         ORESID = ORESID + SCANWT * ORES(INTNDX)**2
         NRESID = NRESID + SCANWT * NRES(INTNDX)**2
      END IF
C
C We jump to here if there was no good data
C
 310  CONTINUE
C
C Now apply these gains to the observed data
C
      DO 300 IVIS = ISTART, IEND
         NEWVIS(IVIS) = VIS(IVIS)
         NEWWT(IVIS) = - ABS(WT(IVIS))
         IF (WT(IVIS).LE.0.0) GO TO 300
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
         IF (CWT(IA1,IA2).GT.0.0) THEN
            NEWWT(IVIS) = ABS(NEWWT(IVIS))
         ELSE
            NFLAG = NFLAG + 1
         END IF
 300  CONTINUE
C
C Now move onto the next integration interval
C
      INTNDX = INTNDX + 1
      ISTART = IEND + 1
C
C Any data left?
C
      IF (ISTART.LE.NVIS) THEN
         IF (INTNDX.GT.NUMINT) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Too many integrations')
            GO TO 999
         END IF
         GO TO 1
      END IF
C
C ********************** End of loop over integrations *****************
C
C Find final fits
C
      IF (TOTALWT.GT.0.0) THEN
         ORESID = SQRT(ORESID/TOTALWT)
         NRESID = SQRT(NRESID/TOTALWT)
      ELSE
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No weight found!')
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
