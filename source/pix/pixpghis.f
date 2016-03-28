C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixpghis.f	1.4    11/7/94
C
      SUBROUTINE PIXPGHIS (NAR, ARRAY, ARRMIN, ARRMAX, NBINS, DEVICE,
     $   X, LABEL)
C
CD Makes a PGPLOT HISTOGRAM of ARRAY on DEVICE
C
C		X, LABEL)
C
C	NAR	INT	input	size of ARRAY
C	ARRAY	REAL	input	Array to be histographed
C	ARRMIN	INT	input	min value to be binned
C	ARRMAX	INT	input	max value to be binned
C	NBINS	INT	input	how many bins?
C	DEVICE	CHAR*(*)input	PGPLOT DEVICE
C	X	CH*(*)	input	X- LABEL
C	LABEL	CH*(*)	input	TITLE-LABEL
C
C Audit trail:
C	And now, with convenient defaults and checking
C				M.A.Holdaway 	Sep 19 1989
C	Tweaked to allow ARRMIN and ARRMAX to be called with a constant
C				D.S.Briggs	Oct 27 1994
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL		ARRAY(*), ARRMAX, ARRMIN
      CHARACTER*(*)	DEVICE, X, LABEL
      INTEGER		NAR, NBINS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXHSGRM')
C
      REAL		SAVE, AMIN, AMAX
      LOGICAL		FLAGMIN, FLAGMAX
      INTEGER		I
      CHARACTER*(SYSMXNAM)	Y
C
C=======================================================================
      IF (ERROR) GO TO 999
C
      FLAGMIN = .FALSE.
      FLAGMAX = .FALSE.
      AMIN = ARRMIN
      AMAX = ARRMAX
      IF (NAR .LE. 0) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'NAR must be .GT. 0')
         GO TO 990
      ENDIF
      WRITE(Y, '(''NUMBER        (Total = '',I6,'')'')') NAR
      IF (LABEL .EQ. ' ') LABEL = 'Histogram'
C
      IF (AMIN .GT. AMAX) THEN
         SAVE = AMIN
         AMIN = AMAX
         AMAX = SAVE
      ENDIF
      IF (AMIN .EQ. 0.) THEN
         FLAGMIN = .TRUE.
         AMIN = .999E+20
         DO 10 I = 1, NAR
            AMIN = MIN( AMIN, ARRAY(I))
 10      CONTINUE
      ENDIF
      IF (AMAX .EQ. 0.) THEN
         FLAGMAX = .TRUE.
         AMAX = -.999E+20
         DO 20 I = 1, NAR
            AMAX = MAX(AMAX, ARRAY(I))
 20      CONTINUE
      ENDIF
      IF (FLAGMIN) THEN
         IF ((AMAX - AMIN) .NE. 0.) THEN
            AMIN = AMIN - .1*(AMAX - AMIN)
         ELSE
            AMIN = AMIN - .1*ABS(AMIN)
         ENDIF
      END IF
      IF (FLAGMAX) THEN
         IF ((AMAX - AMIN) .NE. 0.) THEN
            AMAX = AMAX + .1*(AMAX - AMIN)
         ELSE
            AMAX = AMAX + .1*ABS(AMAX)
         ENDIF
      END IF
      IF (NBINS .LE. 0) NBINS = 10
C
      CALL PGBEGIN (0, DEVICE, 1, 1)
      CALL PGASK(.TRUE.)
      CALL PGHIST  (NAR, ARRAY, AMIN, AMAX, NBINS, 0)
      CALL PGLABEL (X, Y, LABEL)
      CALL PGEND
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
