C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visatmop.f	1.2	7/19/91
C
      SUBROUTINE VISATMOP (VIS, BASL, TIME, WT, NVIS, NANT, TINT,
     $   WATER, REFINDEX, ABSCOEFF, TSKY, LAMBDA, NRMS, SEED, 
     $   NEWVIS, NEWWT, NUMINT, ANTGAIN, 
     $   TGAIN, NFLAG, RMSERR)
C
CD Applies a model ATMOSPHERE to the visibilities
C
C
C
C	VIS	CMPLX	input	Input visibilities
C	BASL	REAL(*)	input	Baselines
C	TIME	REAL(*)	input	Times
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	NANT	INT	input	Number of antennas
C	WATER	REAL	input	MM of Water Vapor above antenna at each time
C	REFINDEX REAL	input	Index of refraction for water
C	ABSCOEFF REAL	input	Absorption coefficient, water
C	TSKY	 REAL	input	Sky Temperature
C	SEED	INT	input	random noise seed
C	TINT	REAL	input	Integration time
C	NEWVIS	CMPLX	output	Output visibilities
C	NEWWT	REAL	output	Output weights
C	NUMINT	INT	input	Number of integrations
C	ANTGAIN	CMPLX	output	Antenna gains
C	TGAIN	REAL	output	Time of antenna gain
C	NFLAG	INT	output	Number of vis. flagged
C	RMSERR	REAL	output	RMS error introduced
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 10 1991
C	Added some debugging statements
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NANT, NUMINT, NFLAG, SEED
      REAL		BASL(*), TIME(*), WT(*), NRMS 
      REAL		RMSERR, NEWWT(*), TINT, TGAIN(*)
      REAL		REFINDEX, ABSCOEFF, TSKY, LAMBDA
      REAL		WATER (NANT, *)
      COMPLEX		VIS(*), NEWVIS(*)
      COMPLEX		ANTGAIN(NANT, *)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCORRP')
C
      INTEGER		IVIS, IA1, IA2, INTNDX
      COMPLEX		CORGAIN, CERR 
      REAL		RNOISE, INOISE, SUMWT
      REAL		PI, MMWATER, PHASE, DPHASE
      PARAMETER		(PI=3.14159274101257)
      INTEGER		ISTART, IEND, NGOOD


C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C ********************** Start of loop over integrations *************
C
      SUMWT = 0.0
      RMSERR = 0.0
      INTNDX = 1
      ISTART = 1
      IEND = 1
C
      NFLAG = 0
C
  1   CONTINUE
C
C Initialize gains
C
      DO 5 IA1 = 1, NANT
         ANTGAIN(IA1,INTNDX) = 0.0
  5   CONTINUE
C
C Loop over data for this integration
C
      NGOOD = 0
      TGAIN(INTNDX) = 0.0
      DO 100 IVIS = ISTART, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         IF (TGAIN(INTNDX).EQ.0.0) THEN
            TGAIN(INTNDX) = TIME(IVIS) + TINT / 2.0
         END IF
         IF (ABS(TIME(IVIS)-TGAIN(INTNDX)+TINT/2.0).LT.(TINT/2.0)) THEN
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
C Make gains
C
      DO 200 IA1 = 1, NANT
         MMWATER = WATER (IA1, INTNDX)
         PHASE = 2.0*PI*(1.0-REFINDEX) * MMWATER/1000./LAMBDA
         IF (SYSDEBUG) THEN
            DPHASE = PHASE * 180./PI
            WRITE (MESSAGE, 1929) MMWATER, DPHASE
 1929       FORMAT ('MMwater, phase : ', 2F10.4)
            CALL MSGPUT (MESSAGE, 'D')
         ENDIF
         ANTGAIN (IA1, INTNDX) = EXP(-MMWATER * ABSCOEFF) *
     $      CMPLX (COS(PHASE), SIN(PHASE))
 200  CONTINUE
C
C Now apply these gains to the observed data
C
      DO 300 IVIS = ISTART, IEND
         IF (WT(IVIS).LE.0.0) THEN
            NEWVIS(IVIS) = 0.
            NEWWT(IVIS) = WT(IVIS)
            GO TO 300
         END IF
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
         CORGAIN = ANTGAIN(IA1, INTNDX) * CONJG(ANTGAIN(IA2, INTNDX))
         CALL UTLGRAND(RNOISE, SEED)
         CALL UTLGRAND(INOISE, SEED)
         CERR = VIS(IVIS) * CORGAIN + NRMS * CMPLX(RNOISE, INOISE)
     1        / SQRT(2.0) - VIS(IVIS)
         NEWVIS(IVIS) = VIS(IVIS) + CERR
         NEWWT(IVIS) = WT(IVIS)
         IF (IA1 .EQ. IA2) THEN
C
C This addative single dish error needs to be scaled to Janskies
C
            NEWVIS(IVIS) = NEWVIS(IVIS) + 
     $         (1.0 - EXP(-ABSCOEFF*MMWATER))*TSKY
         ENDIF
         RMSERR = RMSERR + ABS(CERR)**2 * NEWWT(IVIS) 
         SUMWT = SUMWT + NEWWT(IVIS)
 300  CONTINUE
C
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
         IF (INTNDX.GT.NUMINT) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Too many integrations')
            GO TO 999
         END IF
         GO TO 1
      END IF
C
C ********************** End of loop over integrations *****************
C
      IF (SUMWT.GT.0.0) THEN
         RMSERR = SQRT(RMSERR/SUMWT)
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
