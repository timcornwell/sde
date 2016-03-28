C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vistrppi.f	1.5    06 Sep 1996
C
      SUBROUTINE VISTRPPI (VIS, MVIS, BASL, TIME, WT, MWT, NVIS, 
     1   NANT, TINT, NEWVIS, NEWWT, NUMINT, TGAIN,
     2   ORES, NRES, NFLAG, ORESID, NRESID, MODE)
C
CD Find estimates of visibility function from triple product data
C i.e. uses closure phase information. Uses the model data MVIS as
C an initial estimate for the least-squares routine. 
C Called from VISTRP.
C
C     2   ORES, NRES, NFLAG, ORESID, NRESID, MODE)
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
C	NEWVIS	CMPLX	output	Output visibilities
C	NEWWT	REAL	output	Output weights
C	NUMINT	INT	input	Number of integrations
C	TGAIN	REAL	output	Gain times
C	ORES	REAL	output	Original residuals for each int.
C	NRES	REAL	output	Final residuals for each int.
C	NFLAG	INT	output	Number of vis. flagged
C	ORESID	REAL	output	Residual before correction
C	NRESID	REAL	output	Residual after correction
C      MODE    CH*(*)  input   Mode of solution
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C      Added MODE
C				T.J.Cornwell	March 27 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NANT, NUMINT, NFLAG
      REAL		BASL(*), TIME(*), WT(*), MWT(*), TGAIN(*)
      REAL		NEWWT(*), ORESID, NRESID, TINT
      REAL		ORES(*), NRES(*)
      COMPLEX		VIS(*), MVIS(*), NEWVIS(*)
      CHARACTER*(*)     MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISTRPPI')
C
      CHARACTER*12	STRTIMCH
      INTEGER		IVIS, NCOR, IA1, IA2, IA3, INTNDX
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 28)
      COMPLEX		TRPVIS(MAXNANT,MAXNANT,MAXNANT)
      REAL		TRPWT(MAXNANT,MAXNANT,MAXNANT)
      INTEGER		NSUM(MAXNANT, MAXNANT, MAXNANT)
      COMPLEX		CVIS(MAXNANT,MAXNANT)
      REAL		CWT(MAXNANT,MAXNANT)
      COMPLEX		SVIS(MAXNANT,MAXNANT)
      REAL		SWT(MAXNANT,MAXNANT)
      INTEGER		SNUM(MAXNANT,MAXNANT)
      REAL		IRESID, SCANWT, TOTALWT, TLAST, THISWT
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
      WRITE (MESSAGE, 1000) INTNDX, NUMINT
 1000 FORMAT ('Solving integration ',I4,' of ',I4)
      CALL MSGPUT (MESSAGE, 'I')
C
C Initialize accumulators
C
      DO 10 IA2 = 1, NANT
         DO 11 IA1 = 1, NANT
            CVIS(IA1, IA2) = 0.0
            SVIS(IA1, IA2) = 0.0
            CWT(IA1, IA2) = 0.0
  11     CONTINUE
  10  CONTINUE
C
C Reset accumulators
C
      DO 270 IA3 = 1, NANT
         DO 260 IA2 = 1, NANT
            DO 250 IA1= 1, NANT
               TRPVIS(IA1,IA2,IA3) = 0.0
               TRPWT(IA1,IA2,IA3) = 0.0
               NSUM(IA1,IA2,IA3) = 0
 250        CONTINUE
 260     CONTINUE
 270  CONTINUE
C
C Loop over data for this integration
C
      NGOOD = 0
      TGAIN(INTNDX) = 0.0
      DO 100 IVIS = ISTART, NVIS
         IF ((WT(IVIS).LE.0.0).OR.(MWT(IVIS).LE.0.0).OR.
     1      (ABS(MVIS(IVIS)).EQ.0.0)) GO TO 100
         IF (TGAIN(INTNDX).EQ.0.0) THEN
            TGAIN(INTNDX) = TIME(IVIS) + TINT / 2.0
            TLAST = TIME(IVIS)
         END IF
C
C Is this still part of the same integration?
C
         IF ((TIME(IVIS)-TGAIN(INTNDX)).GT.TINT/2.0) THEN
C
C No: step back one and go to the bi-spectrum solution
C
            IEND = IVIS - 1
            GO TO 120
         ELSE
C
C Yes: same integration: is it the same time stamp as before?
C 
            IF (TIME(IVIS).EQ.TLAST) THEN
C
C Same time: accumulate in arrays
C
               NGOOD = NGOOD + 1
               IA1 = NINT(BASL(IVIS)/256.0)
               IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
               CVIS(IA1, IA2) = VIS(IVIS)
               CVIS(IA2, IA1) = CONJG(VIS(IVIS))
               IF(MODE.EQ.'AMPPHI') THEN
                  SVIS(IA1, IA2) = MVIS(IVIS)
               ELSE
                  SVIS(IA1,IA2) = ABS(VIS(IVIS)) * MVIS(IVIS) /
     $               ABS(MVIS(IVIS))
               END IF
               SVIS(IA2, IA1) = CONJG(MVIS(IVIS))
               CWT(IA1, IA2) = WT(IVIS)
               CWT(IA2, IA1) = WT(IVIS)
           END IF
C
C Was the point just added the last point in the time stamp?
C
           IF ((IVIS.EQ.NVIS).OR.(TIME(IVIS+1).GT.TLAST)) THEN
C
C End of time stamp: average into Bi-Spectrum arrays and then
C continue loop until the end of this gain interval. Remember to
C reset the start time of the current time stamp and to initialize
C the accumulators
C
               IF(IVIS.LT.NVIS) THEN
                  TLAST = TIME(IVIS+1)
               END IF
               DO 180 IA3 = 1, NANT
                  DO 170 IA2 = 1, NANT
                     DO 160 IA1 = 1, NANT
                        TRPVIS(IA1,IA2,IA3) = TRPVIS(IA1,IA2,IA3) +
     1                     CVIS(IA1,IA2) * CVIS(IA2,IA3) *
     2                     CONJG (CVIS(IA1,IA3))
                        TRPWT(IA1,IA2,IA3) = TRPWT(IA1,IA2,IA3) +
     1                     CWT(IA1,IA2) * CWT(IA2,IA3) * CWT(IA3,IA1)
                        NSUM(IA1,IA2,IA3) = NSUM(IA1,IA2,IA3) + 1
  160                CONTINUE
  170             CONTINUE
  180          CONTINUE
               DO 185 IA2 = 1, NANT
                  DO 190 IA1 = 1, NANT
                     CVIS(IA1,IA2) = 0.0
                     CWT(IA1,IA2)  = 0.0
  190             CONTINUE
  185          CONTINUE
            END IF
         END IF
 100  CONTINUE
C
C The only way we can get here is if we have reached the last point
C
      IEND = NVIS
C
C Can get here for last point or for last point of a gain interval.
C
 120  CONTINUE
C
C If no good data then skip the rest of the steps
C
      IF (NGOOD.EQ.0) GO TO 310
C
C Normalize correctly
C
      THISWT = 0.0
      DO 195 IA3 = 1, NANT
         DO 200 IA2 = 1, NANT
            DO 210 IA1= 1, NANT
               IF (NSUM(IA1,IA2,IA3).GT.0) THEN
                  TRPVIS(IA1,IA2,IA3) = TRPVIS(IA1,IA2,IA3) / 
     1                 NSUM(IA1,IA2,IA3)
                  TRPWT(IA1,IA2,IA3) = TRPWT(IA1,IA2,IA3) / 
     1               NSUM(IA1,IA2,IA3)
                  THISWT = THISWT + TRPWT(IA1,IA2,IA3)
               END IF
 210        CONTINUE
 200     CONTINUE
 195  CONTINUE
C
      IF(THISWT.EQ.0.0) THEN
        WRITE (MESSAGE, 1100)
 1100   FORMAT ('Summed weighted zero')
        CALL MSGPUT (MESSAGE, 'I')
        GO TO 310
      END IF
C
C Do solution for this set of data
C
      CALL TRPSOLVE (NANT, NANT, TRPVIS, TRPWT, SVIS, SWT, MODE,
     $   ORES(INTNDX), NRES(INTNDX), SCANWT)
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
C Now apply these phases to the observed data
C
      DO 300 IVIS = ISTART, IEND
         NEWVIS(IVIS) = VIS(IVIS)
         NEWWT(IVIS) = - ABS(WT(IVIS))
         IF (WT(IVIS).LE.0.0) GO TO 300
         IF (MWT(IVIS).LE.0.0) GO TO 300
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
         IF (SWT(IA1,IA2).GT.0.0) THEN
            NEWVIS(IVIS) = SVIS(IA1, IA2) 
            NEWWT(IVIS) = ABS(NEWWT(IVIS))
         ELSE
            NFLAG = NFLAG + 1
         END IF
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
      IF (NFLAG.EQ.NVIS) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'All data flagged')
         GO TO 999
      END IF
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
