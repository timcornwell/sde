C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calantpo.f	1.3    11/7/90
C
      SUBROUTINE CALANTPO (CORPOS, CORWT, NANT, ANTPOS, RESID)
C
CD Calculate antenna position from correlator u,v,w's
C
C
C	CORPOS	REAL	input	Correlator u or v or w
C	NANT	INT	input	Number of antennas
C	ANTPOS	REAL	output	Antenna positions
C	RESID	REAL	output	Error
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include		"stdinc.h"
C
      INTEGER		NANT
      INTEGER		CORWT(NANT,*)
      REAL		CORPOS(NANT,*), ANTPOS(*)
      REAL		RESID
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CALANTPO')
C
      INTEGER		ITER, NITER, IANT, JANT, SUMNANT
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 28)
      REAL		EPS, SUMWT, ORESID, IRESID, COFA
      REAL		TOP(MAXNANT)
      REAL		BOTTOM(MAXNANT)
      DATA		EPS	/1E-5/
      DATA		NITER	/100/
C=====================================================================
      RESID = 0.0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Initialise gains
C
      DO 10 IANT = 1, NANT
         ANTPOS(IANT) = 0.0
  10  CONTINUE
C
C ********************** Start of iteration loop *****************
C
      DO 100 ITER = 1, NITER
C
         DO 21 IANT = 1, NANT
            TOP(IANT) = 0.0
            BOTTOM(IANT) = 0.0
            DO 20 JANT = 1, NANT
               TOP(IANT) = TOP(IANT) + CORWT(IANT, JANT) *
     1            (CORPOS(IANT, JANT) - ANTPOS(IANT) + ANTPOS(JANT))
               BOTTOM(IANT) = BOTTOM(IANT) + CORWT(IANT, JANT)
  20        CONTINUE
  21     CONTINUE
C
C Find new antenna position
C
         COFA = 0.0
         SUMNANT = 0
         DO 30 IANT = 1, NANT
            IF (BOTTOM(IANT).NE.0.0) THEN
               SUMNANT = SUMNANT + 1
               ANTPOS(IANT) = ANTPOS(IANT) + TOP(IANT) / BOTTOM(IANT)
               COFA = COFA + ANTPOS(IANT)
            END IF
  30     CONTINUE
C
         COFA = COFA / SUMNANT
         DO 35 JANT = 1, NANT
            ANTPOS(JANT) = ANTPOS(JANT) - COFA
  35     CONTINUE
C
C Find residual
C
         ORESID = RESID
         SUMWT = 0.0
         RESID = 0.0
         DO 41 JANT = 1, NANT
            DO 40 IANT = 1, NANT
               RESID = RESID + CORWT(IANT, JANT) * 
     1            (ANTPOS(IANT)-ANTPOS(JANT)-CORPOS(IANT, JANT))**2
               SUMWT = SUMWT + CORWT(IANT, JANT)
  40        CONTINUE
  41     CONTINUE
         RESID = SQRT(RESID/SUMWT)
         IF (ITER.EQ.1) THEN
            IRESID = RESID
            ORESID = 2.0*RESID
         END IF
C
         IF (SYSDEBUG) THEN
            WRITE (MESSAGE, 1000) ITER, RESID
 1000       FORMAT ('Iteration ',I3,' residual = ',1PE12.4)
            CALL MSGPUT (MESSAGE, 'D')
         END IF
C
         IF (ABS(ORESID-RESID).LE.EPS*IRESID) GOTO 110
C
  100 CONTINUE
C
C ********************** End of iteration loop *****************
C
      CALL MSGPUT ('Solution for antenna positions did not converge',
     1   'W')
C
  110 CONTINUE
C
      IF (SYSDEBUG) THEN
         DO 210 JANT = 1, NANT
            DO 200 IANT = 1, NANT
               IF (CORWT(IANT, JANT).GT.0) THEN
                  WRITE (MESSAGE, 2000) IANT, JANT, CORPOS(IANT, JANT),
     1               ANTPOS(IANT) - ANTPOS(JANT)
 2000             FORMAT(2(I2,1X),2(F9.3,1X))      
                  CALL MSGPUT (MESSAGE, 'I')
               END IF
 200        CONTINUE
 210     CONTINUE
      END IF
C
C Can jump to here if an error found
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
