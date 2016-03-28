C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)makeconf.f	1.1	 6/28/94
C
      SUBROUTINE SDEMAIN
C
CD Program for non-interactive, ring like array design
C
C Audit trail:
C	Original version
C					M.A.Holdaway	March 15 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MAKECONF')
C
      CHARACTER*(SYSMXNAM)	ANTFILE
      REAL			RADIUS(10), THETA1(10), MINDIST(10)
      REAL			WIGGLE(10)
      INTEGER			NANT(10), SEED
      LOGICAL			RANDOM(10), RANDOM2(10), RADRAN(10)
C
      LOGICAL			FAILED
      INTEGER			NDUMMY, I, J, NTOTAL, NATTEMPT
      INTEGER			IANT, JANT, RING1ANT
      CHARACTER*132     LINE
      REAL		ANGLE, X(200), Y(200), Z, DIAM, D2R
      REAL		AC2M, AD2M, ELONG, DEC, DIST, ARAND
      REAL		ANGLEOLD
CC==================================================================
C
      CALL MSGWELCO ('I make ring-like arrays')
C
C Get input parameters
C
      CALL USRCTL
      CALL USRGETC('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETR('DEC', DEC, 1, NDUMMY)
      CALL USRGETR('ELONG', ELONG, 1, NDUMMY)
      CALL USRGETR('Radius', RADIUS, 10, NDUMMY)
      CALL USRGETR('Theta1', THETA1, 10, NDUMMY)
      CALL USRGETI('NANT', NANT, 10, NDUMMY)
      CALL USRGETL('Random', RANDOM, 10, NDUMMY)
      CALL USRGETL('Random2', RANDOM2, 10, NDUMMY)
      CALL USRGETL('RadRan', RADRAN, 10, NDUMMY)
      CALL USRGETR('Wiggle', WIGGLE, 10, NDUMMY)
      CALL USRGETR('MinDist', MINDIST, 10, NDUMMY)
      CALL USRGETI('Seed', SEED, 1, NDUMMY)
      CALL USRGETR('DIAM', DIAM, 1, NDUMMY)
C
      D2R = ATAN2(1.0, 1.0) / 45.0
      NTOTAL = 0
      DO 50 I = 1, 10
         IF (NANT(I) .LE. 0) GOTO 60
         NTOTAL = NTOTAL + NANT(I)
 50   CONTINUE
 60   CONTINUE
      CALL TXTOPEN (ROUTINE, ANTFILE, 'WRITE')
      WRITE (LINE, *) NTOTAL
      CALL TXTWRITE (ROUTINE, LINE)
      WRITE (LINE, *) DEC
      CALL TXTWRITE (ROUTINE, LINE)
      AC2M = 1.0
      AD2M = 1.0
      WRITE (LINE, *) AC2M, AD2M
      CALL TXTWRITE (ROUTINE, LINE)
C
C  I loops over rings
C  J loops over ants in the rings
C  IANT, JANT loop over antennas
C
      IANT = 0
      Z = 0.0
      DO 100 I = 1, 10
         IF (NANT(I) .LE. 0) GOTO 200
C  first antenna in this ring
         RING1ANT = IANT + 1
         ANGLEOLD =  THETA1(I) - 360.0 / FLOAT(NANT(I))
         DO 90 J = 1, NANT(I)
C
            IANT = IANT + 1
            IF (.NOT. RANDOM(I) .AND. .NOT. RANDOM2(I)) THEN
               ANGLE = THETA1(I) + FLOAT(J-1) * 360.0 / FLOAT(NANT(I))
               X(IANT) = RADIUS(I) * COS( ANGLE * D2R )
               Y(IANT) = RADIUS(I) * SIN( ANGLE * D2R ) * ELONG
            ELSE
               NATTEMPT = 0
C   loop through until we find a good place to put the ant,
C   or until we've tried 100 times and failed
 80            CONTINUE
                  NATTEMPT = NATTEMPT + 1
                  IF (NATTEMPT .GE. 100) THEN
                     CALL ERRREPOR (ROUTINE, ERRLOGIC, 
     $                    'Cant find a place to put it!')
                     GOTO 990
                  ENDIF
                  CALL UTLRAND( ARAND, SEED )
                  IF (RANDOM(I)) THEN
                     ANGLE = ARAND * 4862.3
                  ELSE IF (RANDOM2(I)) THEN
                     ANGLE = MAX( ANGLEOLD, (ANGLEOLD +
     $                     360.0 / FLOAT(NANT(I)) +
     $                    (ARAND - 0.5) * WIGGLE(I)))
                  ENDIF
                  IF (RADRAN(I)) THEN
                     X(IANT) = RADIUS(I) * (1.0 + WIGGLE(I)*
     $                    (ARAND - 0.5)/57.) * COS( ANGLE * D2R )
                     Y(IANT) = RADIUS(I) * (1.0 + WIGGLE(I)*
     $                    (ARAND - 0.5)/57.) * SIN( ANGLE * D2R ) 
     $                    * ELONG
                  ELSE
                     X(IANT) = RADIUS(I) * COS( ANGLE * D2R )
                     Y(IANT) = RADIUS(I) * SIN( ANGLE * D2R )*ELONG
                  ENDIF
C  loop through all other ants
                  FAILED = .FALSE.
                  DO 85 JANT = 1, IANT -1
                     DIST = SQRT( (X(IANT) - X(JANT))**2
     $                    + (Y(IANT) - Y(JANT))**2)
                     IF (DIST .LT. MINDIST(I)) FAILED = .TRUE.
 85               CONTINUE
               IF (FAILED) GOTO 80
               ANGLEOLD = ANGLE 
            ENDIF
            WRITE (LINE, 800) X(IANT), Y(IANT), Z, DIAM
 800        FORMAT( '@     ',4F10.2)
            CALL TXTWRITE (ROUTINE, LINE)
 90      CONTINUE
 100  CONTINUE
 200  CONTINUE
      CALL TXTCLOSE(ROUTINE)
C
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
