C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixsaoscl.f	1.2    11/7/94
C
      SUBROUTINE PIXSAOSCL (AIN, AOUT, N, SMIN, SMAX, STYPE, SPAR)
C
CD Scale a real array in a similar manner to SAOimage
C
C	AIN	R(*)	input	Real array
C	AOUT	R(*)	output	Real array  (may be same as AIN)
C	N	INT	input	size of AIN and AOUT
C	SMIN	REAL	in/out	Minimum value for scaling
C	SMAX	REAL	in/out	maximum value for scaling
C	STYPE	CH*(*)	input	LIN, LOG, WRAP, SQRT, 2LOG
C	SPAR	REAL	in/out	Scaling parameter, varies with type
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	March 12 1994
C	Added type 2LOG
C				D.S.Briggs	Aug 16 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		AIN(*), AOUT(*), SMIN, SMAX, SPAR
      INTEGER		N
      CHARACTER*(*)	STYPE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXSAOSCL')
C
      INTEGER		I
      REAL		RANGE, T, WRAP, SCALE, SCALE1
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF (SMIN.GT.SMAX) THEN
         T = SMIN
         SMIN = SMAX
         SMAX = T
      END IF
      RANGE = SMAX - SMIN
C
      IF (STYPE(1:3).EQ.'LIN') THEN
         CALL MSGPUT ('LINEAR scaling','I')
         DO 100 I = 1, N
            AOUT(I) = MIN(SMAX,MAX(SMIN,AIN(I)))
 100     CONTINUE
C
C One sided log scaling (the way SAOimage does it).  Just log the range
C from min to max with the given parameter.  No special values.
C The SPAR < 0 case in SAOimage doesn't work, so we've not even bothered
C to try here.
C
      ELSE IF (STYPE(1:3).EQ.'LOG') THEN
         IF (SPAR.EQ.0.0) SPAR = 10.0
         WRITE (MESSAGE, 2000) SPAR
 2000    FORMAT ('LOGARITHMIC scaling with parameter',F9.4)
         CALL MSGPUT (MESSAGE, 'I')
         SCALE = RANGE / SPAR
         SCALE1 = (EXP(SPAR) - 1.0) / RANGE
         DO 200 I = 1, N
            AOUT(I) = MIN(SMAX,MAX(SMIN,AIN(I)))
            AOUT(I) = LOG((AOUT(I)-SMIN)*SCALE1 + 1.0)
     $         * SCALE + SMIN
 200     CONTINUE
C
C Two sided log scaling.  This reflects around zero.  It could be
C generalized later, if desired.  Min and Max should both be positive.
C
      ELSE IF (STYPE(1:4).EQ.'2LOG') THEN
         IF (SPAR.EQ.0.0) SPAR = 10.0
         WRITE (MESSAGE, 2500) SPAR
 2500    FORMAT ('2 sided LOGARITHMIC scaling with parameter',F9.4)
         CALL MSGPUT (MESSAGE, 'I')
         SCALE = RANGE / SPAR
         SCALE1 = (EXP(SPAR) - 1.0) / RANGE
         DO 250 I = 1, N
            IF (AIN(I).GE.0.0) THEN
               AOUT(I) = MIN(SMAX,MAX(SMIN,AIN(I)))
               AOUT(I) = LOG((AOUT(I)-SMIN)*SCALE1 + 1.0)
     $            * SCALE + SMIN
            ELSE
               AOUT(I) = MIN(SMAX,MAX(SMIN,-AIN(I)))
               AOUT(I) = LOG((AOUT(I)-SMIN)*SCALE1 + 1.0)
     $            * SCALE + SMIN
               AOUT(I) = -AOUT(I)
            END IF
 250     CONTINUE
      ELSE IF (STYPE(1:4).EQ.'SQRT') THEN
         IF (SPAR.LE.0.0) SPAR = 2.0
         WRITE (MESSAGE, 3000) SPAR
 3000    FORMAT ('ROOT scaling with parameter',F8.4)
         DO 300 I = 1, N
            AOUT(I) = MIN(SMAX,MAX(SMIN,AIN(I)))
            AOUT(I) = ((AOUT(I)-SMIN)/RANGE)**(1/SPAR) * RANGE + SMIN
 300     CONTINUE
      ELSE IF (STYPE(1:4).EQ.'WRAP') THEN
         IF (SPAR.LE.0.0) SPAR = 10.0
         WRITE (MESSAGE, 4000) SPAR
 4000    FORMAT ('WRAP scaling with',F6.2,' cycles')
         CALL MSGPUT (MESSAGE,'I')
         WRAP = RANGE / SPAR
         DO 400 I = 1, N
            AOUT(I) = MIN(SMAX,MAX(SMIN,AIN(I)))
            AOUT(I) = MOD(AOUT(I)-SMIN,WRAP) + SMIN
 400     CONTINUE
         SMAX = SMIN + WRAP
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unknown scaling type')
         GO TO 999
      END IF
C
 999  CONTINUE
      END
