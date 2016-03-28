C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calcorpi.f	1.3    11/7/90
C
      SUBROUTINE CALCORPI (ANTGAIN, NANT, NUMINT, CF, NCF)
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

      PARAMETER		(ROUTINE = 'CALCORPI')
C
      INTEGER		MAXNANT, MAXNCF
      PARAMETER		(MAXNCF = 3)
      PARAMETER		(MAXNANT = 28)
      INTEGER		NUM(MAXNANT,MAXNANT,MAXNCF)
      INTEGER		IA1, IA2, I, ICF, NLAG, ILAG, INTNDX
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
      NLAG = MAX((NCF - 1)/2, 0)
C
      DO 70 ILAG = -NLAG, NLAG
         ICF = ILAG + 1 + NLAG
         DO 60 INTNDX = MAX(1+ILAG,1), MIN(NUMINT-ILAG,NUMINT)
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
      DO 100 ICF = 1, NCF
         DO 90 IA2 = 1, NANT
            DO 80 IA1 = 1, NANT
               IF(NUM(IA1,IA2,ICF).GT.0) THEN
                  CF(IA1,IA2,ICF) = CF(IA1,IA2,ICF) /
     1               NUM(IA1,IA2,ICF)
                  write(*,1000) IA1,IA2,ICF,CF(IA1,IA2,ICF)
 1000             format (3(I4,1X),2(F10.3,1X))
               END IF
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
