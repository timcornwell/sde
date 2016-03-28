C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calcfpix.f	1.3    11/7/90
C
      SUBROUTINE CALCFPIX (ANTGAIN, NANT, NUMINT, CF, NCF)
C
CD Find calibration correlation function
C
C
C	ANTGAIN	CMPLX	input	Antenna gains
C	NANT	INT	input	Number of antennas
C	NUMINT	INT	input	Number of integrations
C	CF	CMPLX	output	Correlation function
C	NCF	INT	input	Number of correlations must be odd
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NANT, NUMINT, NCF
      COMPLEX		ANTGAIN(NANT,NUMINT), CF(NANT, NANT, NCF)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'CALCFPIX')
C
      INTEGER		MAXNANT, MAXNCF
      PARAMETER		(MAXNCF = 4)
      PARAMETER		(MAXNANT = 28)
      INTEGER		NUM(MAXNANT,MAXNANT,MAXNCF)
      INTEGER		NAG(MAXNANT)
      REAL		RMSAG(MAXNANT)
      INTEGER		IA1, IA2, I, ICF, NLAG, ILAG, INTNDX, INTLIM
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 30 ICF = 1, NCF
         DO 20 IA2 = 1, NANT
            DO 10 IA1 = 1, NANT
               CF(IA1, IA2, ICF) = 0.0
               NUM(IA1, IA2, ICF) = 0
  10        CONTINUE
  20     CONTINUE
  30  CONTINUE
C
C Find number of lags
C
      NCF = 4
      NLAG = 1
C
C Do even lags
C
      INTLIM = NUMINT / 2
      DO 70 ILAG = 0, NLAG
         ICF = ILAG + 1
         DO 60 I = 1, MIN(INTLIM-ILAG,INTLIM), 2
            INTNDX = 2 * I
            DO 50 IA2 = 1, NANT
               IF (ABS(ANTGAIN(IA2,INTNDX)).GT.0.0) THEN
                  DO 40 IA1 = 1, NANT
                     IF (ABS(ANTGAIN(IA1,INTNDX)).GT.0.0) THEN
                        NUM(IA1,IA2,ICF) = NUM(IA1,IA2,ICF) + 1
                        CF(IA1,IA2,ICF) = CF(IA1,IA2,ICF) +
     1                     ANTGAIN(IA1,INTNDX) * 
     2                     CONJG(ANTGAIN(IA2,INTNDX+ILAG))
                     END IF
  40              CONTINUE
               END IF
  50        CONTINUE
  60     CONTINUE
  70  CONTINUE
C
C Do odd lags
C
      DO 170 ILAG = 0, NLAG
         DO 165 IA1 = 1, NANT
            RMSAG(IA1) = 0.0
            NAG(IA1) = 0
 165     CONTINUE
         ICF = ILAG + 3
         DO 160 I = 1, MIN(INTLIM-ILAG,INTLIM), 2
            INTNDX = 2 * I - 1
            DO 150 IA2 = 1, NANT
               IF (ABS(ANTGAIN(IA2,INTNDX)).GT.0.0) THEN
                  DO 140 IA1 = 1, NANT
                     IF (ABS(ANTGAIN(IA1,INTNDX)).GT.0.0) THEN
                        NUM(IA1,IA2,ICF) = NUM(IA1,IA2,ICF) + 1
                        CF(IA1,IA2,ICF) = CF(IA1,IA2,ICF) +
     1                     ANTGAIN(IA1,INTNDX) * 
     2                     CONJG(ANTGAIN(IA2,INTNDX+ILAG))
                     END IF
  140             CONTINUE
                  RMSAG(IA2) = RMSAG(IA2) + 
     1               ABS(ANTGAIN(IA2,INTNDX))**2
                  NAG(IA2) = NAG(IA2) + 1
               END IF
  150       CONTINUE
  160    CONTINUE
  170 CONTINUE
C
      DO 175 IA1 = 1, NANT
         IF(NAG(IA1).GT.0) THEN
            RMSAG(IA1) = SQRT(RMSAG(IA1)/NAG(IA1))
         ELSE
            RMSAG(IA1) = 0.0
         END IF
 175  CONTINUE
C
      DO 200 ICF = 1, NCF
         DO 190 IA2 = 1, NANT
            DO 180 IA1 = 1, NANT
               IF(NUM(IA1,IA2,ICF).GT.0) THEN
                  CF(IA1,IA2,ICF) = CF(IA1,IA2,ICF) /
     1               (NUM(IA1,IA2,ICF) * RMSAG(IA1) * RMSAG(IA2))
               END IF
  180       CONTINUE
  190    CONTINUE
  200 CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

