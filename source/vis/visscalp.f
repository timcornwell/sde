C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visscalp.f	1.8    17 Aug 1995
C
      SUBROUTINE VISSCALP (VIS, MVIS, BASL, TIME, WT, MWT, NVIS, 
     1   NANT, TINT, MODE, NEWVIS, NEWWT, NUMINT, ANTGAIN, TGAIN, 
     2   ORES, NRES, NFLAG, ORESID, NRESID)
C
CD Self-Calibrate data, and store antenna gains. Returns global
C residuals and stores residuals for individual integrations.
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
C	ANTGAIN	CMPLX	output	Antenna gains
C	TGAIN	REAL	output	Time of antenna gain
C	ORES	REAL	output	Original residuals for each int.
C	NRES	REAL	output	Final residuals for each int.
C	NFLAG	INT	output	Number of vis. flagged
C	ORESID	REAL	output	Residual before correction
C	NRESID	REAL	output	Residual after correction
C Audit trail:
C	Fixed up logic to make TGAIN the middle point of each
C 	integration (i.e. 0.5*(MAX+MIN)).
C				T.J.Cornwell	Jan 13 1989
C      Now selects data for solution according to the flagging
C      of the model visibilities.
C                              T.J. Cornwell    May 26 1989
C	Added global mode to allow for solution of simple overall
C	calibration errors
C				T.J.Cornwell	Nov 8 1989
C       Mode AMPNORM normalizes mean correction to unity
C				T.J.Cornwell	Sept 5 1992
C	Amplitude solution was wrong because in averaging the data
C	no weighting by SNR was done. Hence low visibility points
C	could contribute spuriously high estimates. This would also
C	have affected phase solutions to a lesser degree
C				T.J.Cornwell	Sept 14 1992
C       Mode AMPNORM was reversed and worked (NOT!)
C				T.J.Cornwell	Sept 15 1992
C	Initialize ORESID and NRESID
C				T.J.Cornwell	Jan 26 1995
C	Upgraded MAXNANT to 40
C				M.A. Holdaway	Aug 17 1995
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NANT, NUMINT, NFLAG
      REAL		BASL(*), TIME(*), WT(*), MWT(*)
      REAL		NEWWT(*), ORESID, NRESID, TINT, TGAIN(*)
      REAL		ORES(*), NRES(*)
      COMPLEX		VIS(*), MVIS(*), NEWVIS(*)
      COMPLEX		ANTGAIN(NANT, *)
      CHARACTER*(*)	MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSCALP')
C
      CHARACTER*12	STRTIMC
      INTEGER		IVIS, NCOR, IA1, IA2, INTNDX
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 40)
      COMPLEX		CVIS(MAXNANT,MAXNANT), CORGAIN
      REAL		CWT(MAXNANT,MAXNANT)
      REAL		IRESID, SCANWT, TOTALWT, TMAX, TMIN, MEANCOR
      INTEGER		ISTART, IEND, NGOOD, NUMCOR
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
      ORESID = 0.0
      NRESID = 0.0
C
      TOTALWT = 0.0
      NFLAG = 0
      NUMCOR=0
      MEANCOR=0.0
C
  1   CONTINUE
C
C Initialize gains
C
      DO 5 IA1 = 1, NANT
         ANTGAIN(IA1,INTNDX) = 0.0
  5   CONTINUE
C
C Initialize accumulators
C
      DO 10 IA2 = 1, NANT
         DO 11 IA1 = 1, NANT
            CVIS(IA1, IA2) = 0.0
            CWT(IA1, IA2) = 0.0
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
      TGAIN(INTNDX) = (TMAX + TMIN)/2.0
C
C If no good data then skip the rest of the steps
C
      IF ((MODE.EQ.'AMPPHI').AND.(NGOOD.LT.4)) GO TO 310
      IF ((MODE.EQ.' ').AND.(NGOOD.LT.3)) GO TO 310
C
C Loop over data for this integration
C
      DO 100 IVIS = ISTART, IEND
         IF ((WT(IVIS).LE.0.0).OR.(MWT(IVIS).LE.0.0).OR.
     1      (ABS(MVIS(IVIS)).EQ.0.0)) GO TO 100
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
         CVIS(IA1, IA2) = CVIS(IA1, IA2) + (VIS(IVIS)/MVIS(IVIS)) *
     $      ABS(MVIS(IVIS))**2 * WT(IVIS)
         CVIS(IA2, IA1) = CVIS(IA2, IA1) + CONJG(VIS(IVIS)/MVIS(IVIS)) *
     $      ABS(MVIS(IVIS))**2 * WT(IVIS)
         CWT(IA1, IA2) = CWT(IA1, IA2) + ABS(MVIS(IVIS))**2 * WT(IVIS)
         CWT(IA2, IA1) = CWT(IA2, IA1) + ABS(MVIS(IVIS))**2 * WT(IVIS)
 100  CONTINUE
C
C Normalize correctly
C
      DO 200 IA2 = 1, NANT
         DO 210 IA1= 1, NANT
            IF (CWT(IA1, IA2).GT.0) THEN
               CVIS(IA1, IA2) = CVIS(IA1, IA2) / CWT(IA1, IA2)
            END IF
 210     CONTINUE
 200  CONTINUE
C
C Do solution for this set of data
C
      IF(MODE.EQ.'GLOBAL') THEN
         CALL CALGBLSO (CVIS, CWT, NANT, ANTGAIN(1,INTNDX),
     1      ORES(INTNDX), NRES(INTNDX), SCANWT)
      ELSE
         CALL CALANTSO (CVIS, CWT, NANT, ANTGAIN(1,INTNDX), MODE, 
     1      ORES(INTNDX), NRES(INTNDX), SCANWT)
      END IF
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
         IF (WT(IVIS).LE.0.0) THEN
            NEWVIS(IVIS) = VIS(IVIS)
            NEWWT(IVIS) = WT(IVIS)
         ELSE
            IA1 = NINT(BASL(IVIS)/256.0)
            IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
            CORGAIN = ANTGAIN(IA1, INTNDX) * CONJG(ANTGAIN(IA2, INTNDX))
            IF (ABS(CORGAIN).NE.0.0) THEN
               MEANCOR=MEANCOR+1.0/ABS(CORGAIN)
               NUMCOR=NUMCOR+1
               NEWVIS(IVIS) = VIS(IVIS) / CORGAIN
               NEWWT(IVIS) = WT(IVIS)
            ELSE
               NEWWT(IVIS) = -WT(IVIS)
               NFLAG = NFLAG + 1
            END IF
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
C Correct for mean amplitude
C
      IF((MODE.EQ.'AMPNORM').OR.(MODE.EQ.'AMPNORMPHI')) THEN
         IF(NUMCOR.EQ.0.0) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'Found no correlators')
            GO TO 999
         ENDIF
         MEANCOR=MEANCOR/NUMCOR
         IF(MEANCOR.EQ.0.0) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'Mean correction is zero')
            GO TO 999
         ELSE
            WRITE (MESSAGE, 1000)  MEANCOR
 1000       FORMAT ('Normalizing mean correction of ',F10.4,
     &         ' back to unity')
            CALL MSGPUT (MESSAGE, 'I')
            DO 400 IVIS = 1, NVIS
               IF (WT(IVIS).LE.0.0) GO TO 400
               NEWVIS(IVIS) = NEWVIS(IVIS)/MEANCOR
 400        CONTINUE
         ENDIF
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
