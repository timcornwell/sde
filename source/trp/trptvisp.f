C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trptvisp.f	1.3	 7/20/92
C
      SUBROUTINE TRPTVISP (TRP, TRIPLE, TTIME, WTT, VIS, BASL,
     $   TIME, WT, MVIS, MWT, NEWVIS, NEWWT, NVIS, NTRP, NANT, ORESID,
     $   NRESID, NFLAG, MODE)
C
C Find estimates of visibility function from triple product data
C i.e. uses closure phase information. Uses the model data MVIS as
C an initial estimate for the least-squares routine. 
C Called from TRPTOVIS
C
C     $   TIME, WT, MVIS, MWT, NEWVIS, NEWWT, NVIS, NTRP, NANT, ORESID,
C     $   NRESID, NFLAG, MODE)
C
C      TRP     CMPLX   input   Input triple products
C      TRIPLE  REAL    input   Triple e.g. 1-2-7
C      TTIME   REAL    input   Triple product time
C      WTT     REAL    input   Weight for triple product
C	VIS	CMPLX	input	Input visibilities (for amplitudes)
C	BASL	REAL(*)	input	Baselines
C	TIME	REAL(*)	input	Times
C	WT	REAL(*)	input	Input weights
C	MVIS	CMPLX	input	Input model visibilities
C	MWT	REAL(*)	input	Input model weights
C      NEWVIS  CMPLX   output  Output visibilities
C      NEWWT   REAL    output  Output weights
C	NTRP	INT	input	Number of triple products
C	NVIS	INT	input	Number of visibilities
C	NANT	INT	input	Number of antennas
C	ORESID	REAL	output	Residual before correction
C	NRESID	REAL	output	Residual after correction
C      NFLAG   INT     output  Number of points flagged
C      MODE    CH*(*)  input   Mode of solution
C
C Audit trail:
C      New routine
C				T.J.Cornwell	April 3 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NANT, NTRP, NFLAG
      REAL		BASL(*), TIME(*), WT(*), MWT(*), TTIME(*)
      REAL		TRIPLE(*), ORESID, NRESID, NEWWT(*), WTT(*)
      COMPLEX		VIS(*), MVIS(*), TRP(*), NEWVIS(*)
      CHARACTER*(*)     MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPTVISP')
C
      CHARACTER*12	STRTIMC
      INTEGER		IVIS, IA1, IA2, IA3, INTNDX
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 28)
C
      COMPLEX		CTRP(MAXNANT,MAXNANT,MAXNANT)
      REAL		CTWT(MAXNANT,MAXNANT,MAXNANT)
      COMPLEX		SVIS(MAXNANT,MAXNANT)
      REAL		SWT(MAXNANT,MAXNANT)
C
      REAL		SCANWT, TOTALWT, TCURRENT, ORES, NRES
      INTEGER		ISTART, IEND, NGOOD, ITRPBEG, ITRP
      LOGICAL		STRMATCH
      REAL		DELTAT
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
      ITRPBEG = 1
      DELTAT = 1.0/ 86400.0
C
      TOTALWT = 0.0
C
  1   CONTINUE
C
C Initialize accumulators
C
      DO 10 IA2 = 1, NANT
         DO 11 IA1 = 1, NANT
            SVIS(IA1, IA2) = 0.0
            SWT(IA1, IA2) = 0.0
  11     CONTINUE
  10  CONTINUE
C
C Reset accumulators
C
      DO 270 IA3 = 1, NANT
         DO 260 IA2 = 1, NANT
            DO 250 IA1= 1, NANT
               CTRP(IA1,IA2,IA3) = 0.0
               CTWT(IA1,IA2,IA3) = 0.0
 250        CONTINUE
 260     CONTINUE
 270  CONTINUE
C
C Loop over data for this integration
C
      NGOOD = 0
      TCURRENT = TIME(ISTART)
      DO 100 IVIS = ISTART, NVIS
C
C Is this still part of the same integration?
C
         IF (ABS(TIME(IVIS)-TCURRENT).GT.DELTAT) THEN
            IEND = IVIS - 1
            GO TO 120
         ELSE
            IEND = IVIS
            IF ((WT(IVIS).LE.0.0).OR.(MWT(IVIS).LE.0.0).OR.
     1         (ABS(MVIS(IVIS)).EQ.0.0)) GO TO 100
            NGOOD = NGOOD + 1
            IA1 = NINT(BASL(IVIS)/256.0)
            IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
            IF(MODE.EQ.'AMPPHI') THEN
               SVIS(IA1, IA2) = MVIS(IVIS)
            ELSE
               SVIS(IA1,IA2) = ABS(VIS(IVIS)) * MVIS(IVIS) /
     $            ABS(MVIS(IVIS))
            END IF
            SVIS(IA2, IA1) = CONJG(SVIS(IA1,IA2))
            SWT(IA1, IA2) = WT(IVIS)
            SWT(IA2, IA1) = WT(IVIS)
         END IF
 100  CONTINUE
C
 120  CONTINUE
C
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 2000) NGOOD
 2000    FORMAT ('There are ',I6,' good points for this timestamp')
         CALL MSGPUT (MESSAGE, 'I')
      ENDIF
C
C If no good data then skip the rest of the steps
C
      IF (NGOOD.EQ.0) GO TO 310
C
C Write out this time range
C
      WRITE (MESSAGE, 1000) INTNDX
 1000 FORMAT ('Solving integration ',I4,', start time = ')
      CALL STRAPPEN (MESSAGE, STRTIMC(TIME(ISTART)))
      CALL MSGPUT (MESSAGE, 'I')
C
C
C Now get the triple product data
C
      DO 180 ITRP = ITRPBEG, NTRP
         IF (ABS(TTIME(ITRP)-TCURRENT).GT.DELTAT) GO TO 181
         IA1 = NINT(TRIPLE(ITRP))/256**2
         IA2 = NINT(TRIPLE(ITRP)-FLOAT(256**2*IA1))/256.0
         IA3 = NINT(TRIPLE(ITRP)-FLOAT(256**2*IA1+256*IA2))
         CTRP(IA1,IA2,IA3) = TRP(ITRP)
         CTWT(IA1,IA2,IA3) = WTT(ITRP)
 180  CONTINUE
 181  CONTINUE
      ITRPBEG = ITRP
C
C Do solution for this set of data
C
      CALL TRPSOLVE (MAXNANT, NANT, CTRP, CTWT, SVIS, SWT, MODE, ORES,
     $    NRES, SCANWT)
      IF (ERROR) THEN
         CALL ERRREASO (MESSAGE)
         IF (STRMATCH(MESSAGE,ERRNTFND)) THEN
            CALL ERRCANCE
         ELSE
            GO TO 990
         END IF
      ELSE
         TOTALWT = TOTALWT + SCANWT
         ORESID = ORESID + SCANWT * ORES**2
         NRESID = NRESID + SCANWT * NRES*2
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
