C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calantso.f	1.6    17 Aug 1995
C
      SUBROUTINE CALANTSO (CORGAIN, CORWT, NANT, ANTGAIN, MODE, 
     1   ORESID, NRESID, SCANWT)
C
CD Calculate antenna gains from correlator gains. The gain on
C entry is used as an initial estimate unless it is zero.
C
C
C	CORGAIN	CMPLX	input	Correlator gains
C	CORWT	REAL	input	Correlator weights: must all be non-
C				negative
C	NANT	INT	input	Number of antennas (not more than 40!)
C	ANTGAIN	CMPLX	output	Estimated antenna gains
C	MODE	CH*(*)	input	Mode ' '|'PHI'
C	ORESID	REAL	output	Error before fit
C	NRESID	REAL	output	Error after fit
C	SCANWT	REAL	output	Total weight for this scan
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added AMP option
C				T.J.Cornwell	August 11 1992
C	Require finer convergence
C				T.J.Cornwell	September 14 1992
C	Upped MAXNANT to 40
C				M.A. Holdaway	Aug 17 1995
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NANT
      COMPLEX		CORGAIN(NANT,*), ANTGAIN(*)
      REAL		CORWT(NANT, *), ORESID, NRESID, SCANWT
      CHARACTER*(*)	MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CALANTSO')
C
      INTEGER		ITER, NITER, IANT, JANT
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 40)
      REAL		EPS, ANTWT, PRESID, GAIN
      COMPLEX		TOP(MAXNANT)
      REAL		BOTTOM(MAXNANT)
      DATA		EPS	/1E-6/
      DATA		NITER	/100/
      DATA		GAIN	/0.5/
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Initialise gain only if the gain on entry is zero
C
      ORESID = 0.0
      NRESID = 0.0
      SCANWT = 0.0
C
C Set initial estimates of gains
C
      DO 10 IANT = 1, NANT
         ANTWT = 0.0
         DO 11 JANT = 1, NANT
            ANTWT = ANTWT + CORWT(IANT, JANT)
  11     CONTINUE
         IF (ANTWT.GT.0.0) THEN
            SCANWT = SCANWT + ANTWT
            IF (ABS(ANTGAIN(IANT)).EQ.0.0) THEN
               DO 12 JANT = 1, NANT
                  ANTGAIN(IANT) = ANTGAIN(IANT) + CORGAIN(IANT, JANT) *
     1               CORWT(IANT, JANT)
  12           CONTINUE
               ANTGAIN(IANT) = ANTGAIN(IANT) / ANTWT
            END IF
         ELSE
            ANTGAIN(IANT) = 0.0
         END IF
  10  CONTINUE
      IF (SCANWT.EQ.0.0) THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE, 'No valid data')
         GO TO 999
      END IF
C
C
      IF (MODE.EQ.' ') THEN
         DO 15 IANT = 1, NANT
            IF(ABS(ANTGAIN(IANT)).NE.0.0) THEN
               ANTGAIN(IANT) = ANTGAIN(IANT)/ABS(ANTGAIN(IANT))
            END IF
  15     CONTINUE
      ELSEIF (MODE.EQ.'AMP') THEN
         DO 16 IANT = 1, NANT
            ANTGAIN(IANT) = ABS(ANTGAIN(IANT))
  16     CONTINUE
      END IF
C
C Find initial rms
C
      PRESID = 0.0
      DO 14 IANT = 1, NANT
         DO 13 JANT = 1, NANT
            ORESID = ORESID + ABS(CORGAIN(IANT, JANT)-1.0)**2 *
     1         CORWT(IANT, JANT)
  13     CONTINUE
  14  CONTINUE
C
      ORESID = SQRT(ORESID/SCANWT)
C
      NRESID = 0.0
      DO 100 ITER = 1, NITER
C
         DO 21 IANT = 1, NANT
            TOP(IANT) = CMPLX (0.0, 0.0)
            BOTTOM(IANT) = 0.0
            DO 20 JANT = 1, NANT
               TOP(IANT) = TOP(IANT) + CORWT(IANT, JANT) * 
     1            ANTGAIN(JANT) * CORGAIN(IANT, JANT)
               BOTTOM(IANT) = BOTTOM(IANT) + ABS(ANTGAIN(JANT))**2 *
     1            CORWT(IANT, JANT)
  20        CONTINUE
  21     CONTINUE
C
C Find new antenna gain
C
         DO 30 IANT = 1, NANT
            IF (BOTTOM(IANT).NE.0.0) THEN
               ANTGAIN(IANT) = (1.0-GAIN) * ANTGAIN(IANT) +
     1            GAIN * TOP(IANT) / BOTTOM(IANT)
            END IF
  30     CONTINUE
C
C Is this a phase-only solution?
C
      IF (MODE.EQ.' ') THEN
         DO 35 IANT = 1, NANT
            IF(ABS(ANTGAIN(IANT)).NE.0.0) THEN
               ANTGAIN(IANT) = ANTGAIN(IANT)/ABS(ANTGAIN(IANT))
            END IF
  35     CONTINUE
      ELSEIF (MODE.EQ.'AMP') THEN
         DO 36 IANT = 1, NANT
            ANTGAIN(IANT) = ABS(ANTGAIN(IANT))
  36     CONTINUE
      END IF
C
C Find residual
C
         PRESID = NRESID
         NRESID = 0.0
         ANTWT = 0.0
         DO 41 JANT = 1, NANT
            DO 40 IANT = 1, NANT
               NRESID = NRESID + CORWT(IANT, JANT) *
     1            ABS(ANTGAIN(IANT)*CONJG(ANTGAIN(JANT)) -
     2            CORGAIN(IANT, JANT))**2
               ANTWT = ANTWT + CORWT(IANT, JANT)
  40        CONTINUE
  41     CONTINUE
         NRESID = SQRT(NRESID/ANTWT)
C
         IF (SYSDEBUG) THEN
            WRITE (MESSAGE, 1000) ITER, NRESID
 1000       FORMAT ('Iteration ',I3,' residual = ',1PE12.4)
            CALL MSGPUT (MESSAGE, 'D')
         END IF
C
         IF (ABS(NRESID-PRESID).LE.EPS*ORESID) GOTO 110
C
  100 CONTINUE
C
  110 CONTINUE
C
C Can jump to here if an error found
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
