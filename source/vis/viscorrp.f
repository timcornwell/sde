C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)viscorrp.f	1.6	 10/7/93
C
      SUBROUTINE VISCORRP (VIS, PHRMS, GRMS, GDRIFT, NRMS, SEED, 
     $   BASL, TIME, 
     $   WT, NVIS, NANT, TINT, NEWVIS, NEWWT, NUMINT, ANTGAIN, 
     $   TGAIN, NFLAG, RMSERR)
C
CD Corrupt data, and store antenna gains. Returns global
C residuals and stores residuals for individual integrations.
C
C	VIS	CMPLX	input	Input visibilities
C	PHRMS	REAL	input	Rms phase per telescope
C	GRMS	REAL	input	Rms complex antenna gain
C	GDRIFT	REAL	input	Drift in gains
C	NRMS	REAL	input	Rms noise per correlator
C	BASL	REAL(*)	input	Baselines
C	TIME	REAL(*)	input	Times
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	NANT	INT	input	Number of antennas
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
C				T.J.Cornwell	Jan 5 1989
C	Fixed logic re. WT for out=in and add corruption rather than
C       make residuals.
C				R.Braun 	Jul 10 1989
C	Added GRMS, fluctuations in complex antenna gains,
C	as a fraction of the gain (like .05)
C				M.A.Holdaway	May 22 1990
C	SEED is now passed into this routine
C				M.A.Holdaway	April 25 1991
C	A negative value of NRMS means to add |NRMS|/sqrt(wt)
C	noise to each visibility
C				D.S.Briggs	Mar 23 1993
C	Modified time binning calculations to conform to the rest
C	of the SDE world.  Like it or not, the first element of an
C	array always starts a time bin, whether or not it has a
C	positive weight.  Added fuzz to time calculations to
C	try and dodge rounding error.  Bugfix.
C				D.S.Briggs	Oct 6 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NANT, NUMINT, NFLAG, SEED
      REAL		BASL(*), TIME(*), WT(*), PHRMS, GRMS, NRMS 
      REAL		GDRIFT, RMSERR, NEWWT(*), TINT, TGAIN(*)
      COMPLEX		VIS(*), NEWVIS(*)
      COMPLEX		ANTGAIN(NANT, *)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCORRP')
C
      CHARACTER*12	STRTIMC
      INTEGER		IVIS, NCOR, IA1, IA2, INTNDX
      COMPLEX		CORGAIN, CERR 
      REAL		PHASE, P, RNOISE, INOISE, SUMWT
      REAL		PI, RGAIN, IGAIN, TISMALL
      PARAMETER		(PI=3.14159274101257)
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
      SUMWT = 0.0
      RMSERR = 0.0
      INTNDX = 1
      ISTART = 1
      IEND = 1
      TGAIN(1) = TIME(1) + TINT / 2.0
      TISMALL = TINT - TINT*5.0E-5
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
         IF (ABS(TIME(IVIS)-TGAIN(INTNDX)+TINT/2.0).LT.(TISMALL)) THEN
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
         CALL UTLGRAND (P, SEED)
         CALL UTLGRAND(RGAIN, SEED)
         CALL UTLGRAND(IGAIN, SEED)
         PHASE = 2.0 * PI * (PHRMS / 360.0) * P
         ANTGAIN(IA1,INTNDX) = CMPLX(COS(PHASE), SIN(PHASE))
     $      * (1. + GRMS * CMPLX(RGAIN, IGAIN) )
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
         IF (IA1 .EQ. IA2) THEN
            CORGAIN = CORGAIN * (1 + GDRIFT) 
         ENDIF
         CALL UTLGRAND(RNOISE, SEED)
         CALL UTLGRAND(INOISE, SEED)
         IF (NRMS.GT.0) THEN
            CERR = VIS(IVIS) * CORGAIN - VIS(IVIS) +
     $         NRMS * CMPLX(RNOISE, INOISE) / SQRT(2.0)
         ELSE
            CERR = VIS(IVIS) * CORGAIN - VIS(IVIS) +
     $         ABS(NRMS) * CMPLX(RNOISE, INOISE) / SQRT(2.0*WT(IVIS))
         END IF
         NEWVIS(IVIS) = VIS(IVIS) + CERR
         NEWWT(IVIS) = WT(IVIS)
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
