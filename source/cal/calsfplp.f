C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calsfplp.f	1.3    11/7/90
C
      SUBROUTINE CALSFPLP (SF, NANT, NLAG, U, V, XSCALE, YSCALE, UMAX,
     1   VMAX, STRING)
C
CD Plot calibration structure function
C
C
C	SF	REAL	input	Structure function
C	NANT	INT	input	Number of antennas
C	NLAG	INT	input	Number of correlations
C	U, V	REAL	input	Antenna locations
C	XSCALE	REAL	input	Scale factors for x, y axes
C	UMAX	REAL	input	Maximum U value
C	STRING	CH*(*)	input	String to label with
C Audit trail:
C	New version
C				T.J.Cornwell	Jan 16 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	STRING
      INTEGER		NANT, NLAG
      REAL		SF(NANT, NANT, *), U(*), V(*), XSCALE, YSCALE
      REAL		UMAX, VMAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CALSFPLP')
C
      INTEGER		IA1, IA2, ILAG
      REAL		X, Y
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 ILAG = 1, NLAG
C
         CALL PGENV (-2.2*UMAX, 2.2*UMAX, -2.2*VMAX, 2.2*VMAX, 1, 0)
         IF (ERROR) GO TO 990
         CALL PGLABEL ('U position', 'V position', STRING)
         WRITE (MESSAGE, 1000) ILAG-1
 1000    FORMAT ('Antenna Gain structure function for lag ',I2)
         CALL PGMTEXT ('T', 1.0, 0.1, 0.0, MESSAGE)
         DO 20 IA2 = 1, NANT
            DO 15 IA1 = 1, NANT
               IF (SF(IA1,IA2,ILAG).GT.0.0) THEN
                  X = - (U(IA2) - U(IA1))
                  Y = V(IA2) - V(IA1)
                  CALL PGMOVE (X,Y)
                  Y = Y + YSCALE * SF(IA1, IA2, ILAG)
                  CALL PGDRAW (X,Y)
               END IF
  15        CONTINUE
  20     CONTINUE
         X = -0.95*2.2*UMAX
         Y = -0.95*2.2*VMAX
         CALL PGMOVE (X,Y)
         Y = Y + YSCALE
         CALL PGDRAW (X,Y)
         IF (ERROR) GO TO 990
         CALL PGIDEN
  10  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

