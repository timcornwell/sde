C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calgblso.f	1.3    11/7/90
C
      SUBROUTINE CALGBLSO (CORGAIN, CORWT, NANT, ANTGAIN,
     1   ORESID, NRESID, SCANWT)
C
CD Calculate single global gains from correlator gains. The gain on
C entry is used as an initial estimate unless it is zero.
C
C
C	CORGAIN	CMPLX	input	Correlator gains
C	CORWT	REAL	input	Correlator weights: must all be non-
C				negative
C	NANT	INT	input	Number of antennas (not more than 28)
C	ANTGAIN	CMPLX	output	Estimated antenna gains
C	ORESID	REAL	output	Error before fit
C	NRESID	REAL	output	Error after fit
C	SCANWT	REAL	output	Total weight for this scan
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NANT
      COMPLEX		CORGAIN(NANT,*), ANTGAIN(*)
      REAL		CORWT(NANT, *), ORESID, NRESID, SCANWT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CALGBLSO')
C
      INTEGER		IANT, JANT
      REAL		TOP
      COMPLEX		GBLGAIN, BOTTOM
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      TOP = 0.0
      BOTTOM = CMPLX(0.0,0.0)
      DO 21 IANT = 1, NANT
         DO 20 JANT = 1, NANT
            IF(CORWT(IANT,JANT).LE.0.0) GO TO 20
            TOP=TOP+CORWT(IANT, JANT)*ABS(CORGAIN(IANT, JANT))**2
            BOTTOM=BOTTOM+CORWT(IANT,JANT)*CONJG(CORGAIN(IANT,JANT))
  20     CONTINUE
  21  CONTINUE
      IF(ABS(BOTTOM).NE.0.0) THEN
         GBLGAIN = TOP / BOTTOM
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'summed correlator gain is zero')
         GO TO 999
      END IF
C
C Find new antenna gain
C
      DO 30 IANT = 1, NANT
         ANTGAIN(IANT) = SQRT(ABS(GBLGAIN))
  30  CONTINUE
C
C Find residuals
C
      ORESID = 0.0
      NRESID = 0.0
      SCANWT = 0.0
      DO 41 JANT = 1, NANT
         DO 40 IANT = 1, NANT
            IF(CORWT(IANT,JANT).LE.0.0) GO TO 40
            ORESID = ORESID + CORWT(IANT, JANT) *
     $         ABS(CORGAIN(IANT, JANT)-1.0)**2
            NRESID = NRESID + CORWT(IANT, JANT) *
     $         ABS(CORGAIN(IANT, JANT)-GBLGAIN)**2
            SCANWT = SCANWT + CORWT(IANT, JANT)
  40     CONTINUE
  41  CONTINUE
      ORESID = SQRT(ORESID/SCANWT)
      NRESID = SQRT(NRESID/SCANWT)
C
C Can jump to here if an error found
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
