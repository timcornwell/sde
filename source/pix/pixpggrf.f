C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixpggrf.f	1.5	 7/21/94
C
      SUBROUTINE PIXPGGRF (NPTS, ARRX, ARRY, TXMIN, TXMAX, TYMIN, TYMAX,
     $   DEVICE, XLAB, YLAB, LABEL, STYLE, AXIS)
C
C Makes a PGPLOT X,Y graph on DEVICE
C
C	NPTS	INT	input	size of ARRX
C	ARRX	REAL	input	X ARRAY graphed
C	ARRY	REAL	input	Y ARRAY graphed
C	XMIN	INT	input	min value to be GRAPHED
C	XMAX	INT	input	max value to be GRAPHED
C	DEVICE	CHAR*(*)input	PGPLOT DEVICE
C	XLAB	CH*(*)	input	X- LABEL
C	LABEL	CH*(*)	input	TITLE-LABEL
C	STYLE	INT	input	line=0; graph markers=1-31
C	AXIS	INT	input	PGPLOT style (0 = normal, 30 = log-log)
C
C Audit trail:
C	And now, with convenient defaults and checking
C				M.A.Holdaway 	Sep 19 1989
C	Added AXIS argument.  Log support added.  Autoscaling tweaked.
C				D.S.Briggs	June 15 1992
C	Was ignoring suggested XMAX, YMAX, etc
C				M.A.Holdaway	Sept 21 1992
C	New Feature: STYLE = -5049 will start with character
C	number 49, plot the next point with 50, and so on
C	up to 53 (cycle of 5).
C	Convenient starting points: 65 = 'A', 97 = 'a', 49='1'
C				M.A.Holdaway	Oct 16 1992
C	Newer feature:  STYLE -1 through -5 corresponds to line styles,
C	just as in GENPLOT.  This doesn't conflict with the previous
C	use of negative STYLE.
C				D.S.Briggs	July 20 1994
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL		ARRX(*), ARRY(*), TXMAX, TXMIN, TYMAX, TYMIN
      CHARACTER*(*)	DEVICE, XLAB, YLAB, LABEL
      INTEGER		NPTS, STYLE, AXIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXPGGRF')
C
      REAL		SAVE, XMIN, XMAX, YMIN, YMAX
      INTEGER		I, PGBEGIN, ICYCLE, INOW, IBASE
C      REAL		PGRND
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      XMIN = TXMIN
      XMAX = TXMAX
      YMIN = TYMIN
      YMAX = TYMAX
C
      IF (NPTS .LE. 0) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'NPTS must be .GT. 0')
         GO TO 990
      ENDIF
C
C Do logs, if needed
C
      IF ((AXIS.EQ.10).OR.(AXIS.EQ.30)) THEN
         DO 5 I = 1, NPTS
            ARRX(I) = LOG10(ARRX(I))
 5       CONTINUE
         IF(XMIN.NE.0.0) XMIN = LOG10(XMIN)
         IF(XMAX.NE.0.0) XMAX = LOG10(XMAX)
      END IF
C
      IF ((AXIS.EQ.20).OR.(AXIS.EQ.30)) THEN
         DO 6 I = 1, NPTS
            ARRY(I) = LOG10(ARRY(I))
 6       CONTINUE
         IF(YMIN.NE.0.0) YMIN = LOG10(YMIN)
         IF(YMAX.NE.0.0) YMAX = LOG10(YMAX)
      END IF
C
C take care of X defaults
C
      IF (XMIN .GT. XMAX) THEN
         SAVE = XMIN
         XMIN = XMAX
         XMAX = SAVE
      ENDIF
      IF (XMIN .EQ. 0.) THEN
         XMIN = ARRX(1)
         DO 10 I = 1, NPTS
            XMIN = MIN( XMIN, ARRX(I))
 10      CONTINUE
         XMAX = ARRX(1)
         DO 15 I = 1, NPTS
            XMAX = MAX(XMAX, ARRX(I))
 15      CONTINUE
         XMIN = XMIN - .1*(XMAX - XMIN)
         XMAX = XMAX + .1*(XMAX - XMIN)
      ENDIF
      IF (XMAX .EQ. 0.) THEN
         XMAX = ARRX(1)
         DO 20 I = 1, NPTS
            XMAX = MAX(XMAX, ARRX(I))
 20      CONTINUE
         IF (XMAX .GT. 0.0) THEN
            XMAX = XMAX * 1.1
         ELSE
            XMAX = XMAX * .9
         ENDIF
      ENDIF
C
C take care of Y defaults
C
      IF (YMIN .GT. YMAX) THEN
         SAVE = YMIN
         YMIN = YMAX
         YMAX = SAVE
      ENDIF
      IF (YMIN .EQ. 0.) THEN
         YMIN = ARRY(1)
         DO 30 I = 1, NPTS
            YMIN = MIN( YMIN, ARRY(I))
 30      CONTINUE
         YMAX = ARRY(1)
         DO 35 I = 1, NPTS
            YMAX = MAX(YMAX, ARRY(I))
 35      CONTINUE
         YMIN = YMIN - .1*(YMAX-YMIN)
         YMAX = YMAX + .1*(YMAX-YMIN)
      ENDIF
      IF (YMAX .EQ. 0.) THEN
         YMAX = ARRY(1)
         DO 40 I = 1, NPTS
            YMAX = MAX(YMAX, ARRY(I))
 40      CONTINUE
         IF (YMAX .GT. 0.0) THEN
            YMAX = YMAX * 1.1
         ELSE
            YMAX = YMAX * .9
         ENDIF
      ENDIF
C
C We really should decide what we want here -- this are previous historical
C  attempts at reasonable defaults.
C
C      YMAX = PGRND(YMAX)
C      XMAX = PGRND(XMAX)
C      XMIN = -PGRND(-XMIN)
C      YMIN = -PGRND(-YMIN)
C      IF (YMIN .GT. 0. .AND. YMIN/YMAX .LE. .2) YMIN = 0.0
C      IF (XMIN .GT. 0. .AND. XMIN/XMAX .LE. .2) XMIN = 0.0
C
      IF ((ABS(XMIN) .LE. 1.E-10).AND.(AXIS.LT.10)) XMIN = 0.0
      IF ((ABS(YMIN) .LE. 1.E-10).AND.(AXIS.LT.10)) YMIN = 0.0
C
C now graph your brains out
C
      I = PGBEGIN (0, DEVICE, 1, 1)
      CALL PGENV   (XMIN,XMAX,YMIN,YMAX, 0, AXIS)
      CALL PGLABEL (XLAB, YLAB, LABEL)
      IF (STYLE .EQ. 0) THEN
         CALL PGLINE  (NPTS, ARRX, ARRY)
      ELSE IF (STYLE .LT. 0) THEN
         IF (STYLE.LT.-5) THEN
            ICYCLE = ABS(STYLE)/1000
            IBASE = ABS(STYLE) - ICYCLE*1000
            DO 100 I = 1, NPTS
               INOW = IBASE + MOD (I-1, ICYCLE)
               CALL PGPOINT (1, ARRX(I), ARRY(I), INOW)
 100        CONTINUE
         ELSE
            CALL PGQLS (I)
            CALL PGSLS (ABS(STYLE))
            CALL PGLINE  (NPTS, ARRX, ARRY)
            CALL PGSLS (I)
         END IF
      ELSE
         CALL PGPOINT (NPTS, ARRX, ARRY, STYLE)
      END IF
      CALL PGIDEN
      CALL PGEND
C
C Restore original array values, if needed
C
      IF ((AXIS.EQ.10).OR.(AXIS.EQ.30)) THEN
         DO 50 I = 1, NPTS
            ARRX(I) = 10.0**ARRX(I)
 50      CONTINUE
      END IF
C
      IF ((AXIS.EQ.20).OR.(AXIS.EQ.30)) THEN
         DO 60 I = 1, NPTS
            ARRY(I) = 10.0**ARRY(I)
 60      CONTINUE
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
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
C      REAL FUNCTION PGRND (X)
C
CD For some reason, PGPLOT's PGROUND doesn't work, so....
C
CS Arguments: Y = PGRND ( X )
CS
CS	X	REAL	INPUT	What gets rounded up to "nice" number
CA
CA Audit trail:
CA	Original version: Audit trail comments go on this line
CA	and successive lines
CA				M.A. Holdaway	25 Oct 1989
CE
C-----------------------------------------------------------------------
C
C#include		"stdinc.h"
C
C      REAL		X
C
C      CHARACTER*(*)	ROUTINE
C      PARAMETER		(ROUTINE = 'PGRND')
C
C      INTEGER		LOGX, I
C      REAL		XMAX(9), ROUND
C
C      INTEGER		GINT
C=======================================================================
C
C If an error on input then exit immediately
C
C      IF (ERROR) GO TO 999
C      
C      IF (X .EQ. 0.0) THEN
C         ROUND = 0.0
C      ELSE
C         LOGX = GINT( LOG10(ABS(X)) )
C         DO 20 I = 1, 9
C            XMAX(I) = SIGN( FLOAT(I) * 10.**(LOGX), X )
C 20      CONTINUE
C         ROUND = XMAX(9)
C         DO 40 I = 8, 1, -1
C            IF (XMAX(I) .GT. X)  ROUND = XMAX(I)
C 40      CONTINUE
C      ENDIF
C
C Can jump to here if an error found
C
C  990 IF (ERROR) THEN
C         CALL ERRTRACE (ROUTINE)
C      END IF
C
C  999 CONTINUE
C      PGRND = ROUND
C      RETURN
C      END






