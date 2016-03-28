C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trpsolve.f	1.2	 7/20/92
C
      SUBROUTINE TRPSOLVE (MNANT, NANT, TRPARRY, TRPWT, TRPAINV, 
     $   TRPAWT, MODE, ORES, NRES, SUMWT)
C
C Solves for u,v data from triple product data. TRPAINV must contain
C an initial estimate on entrance. If MODE = 'AMPPHI' then both amplitude
C and phase are solved for, otherwise the amplitude of TRPAINV is 
C assumed to be correct
C
C     $   TRPAWT, MODE, ORES, NRES, SUMWT)
C	NANT	INT	input	Number of antennas
C	TRPARRY	CMPLX	input	Input triple product data
C	TRPWT	REAL	input	Weights
C	TRPAINV	CMPLX	output	Solved u,v data
C	TRPAWT	REAL	output	Weights of u,v data
C      MODE    CH*(*)  input   Mode of solution 'AMPPHI' | ' '
C	ORES	REAL	output	Original Residual
C	NRES	REAL	output	New Residual
C	SUMWT	REAL	output	Sum of weights
C
C Audit trail:
C	Still need scalar directive for new version of fc
C				T.J.Cornwell	Feb 21 1989
C	Renamed from BSSOLVE
C				T.J. Cornwell	March 3 1989
C	Changed to simpler linear stepping
C				T.J. Cornwell	March 15 1989
C      Added MODE
C				T.J. Cornwell	March 27 1989
C      Added MNANT
C                              T.J. Cornwell   April 4 1989
C
C
C-----------------------------------------------------------------------
C
#include	"stdinc.h"
C
C Arguments
C
      INTEGER	NANT, MNANT
      COMPLEX	TRPARRY(MNANT,MNANT,*)
      REAL	TRPWT (MNANT,MNANT,*)
      COMPLEX	TRPAINV(MNANT,*)
      REAL	TRPAWT (MNANT,*)
      REAL	ORES, NRES, SUMWT
      CHARACTER*(*)    MODE
C
      INTEGER	MAXNANT
      PARAMETER	(MAXNANT=28)
      COMPLEX	TTRPAINV(MAXNANT,MAXNANT)
      COMPLEX	DAINV(MAXNANT,MAXNANT)
      REAL	HAINV(MAXNANT,MAXNANT)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPSOLVE')
C
C Local variables
C
      INTEGER	IA1, IA2, IA3
      INTEGER	ITER, NITER
      REAL	OLDN, N1, N2, INITN, N, NPRED, EPS, GAIN, SUM
      REAL	CURV, STEP
      COMPLEX	TRPAERR, CSTEP
C
      DATA NITER/100/
      DATA GAIN/0.5/
      DATA EPS / 1E-4/
C-----------------------------------------------------------------------
      IF (ERROR) GO TO 999
C
      N = 0.0
C
C Correct weighting. 
C
      SUMWT = 0.0
      DO 30 IA3 = 1, NANT
         DO 20 IA2 = 1, NANT
               DO 10 IA1 = 1, NANT
                  SUMWT = SUMWT + TRPWT(IA1,IA2,IA3)
  10           CONTINUE
  20      CONTINUE
  30   CONTINUE
C
C Initialise arrays
C
      DO 50 IA2 = 1, NANT
         DO 40 IA1 = 1, NANT
            DAINV(IA1,IA2) = 0.0
            HAINV(IA1,IA2) = 0.0
  40     CONTINUE
  50  CONTINUE
C
      IF (SUMWT.EQ.0.0) THEN
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 'Zero summed weight')
         GO TO 999
      END IF
      DO 80 IA3 = 1, NANT
         DO 70 IA2 = 1, NANT
            DO 60 IA1 = 1, NANT
               TRPWT(IA1,IA2,IA3) = TRPWT(IA1,IA2,IA3) / SUMWT
  60        CONTINUE
  70     CONTINUE
  80  CONTINUE
C
C Find initial misfit
C
      CALL TRPCALN (NANT, TRPARRY, TRPWT, TRPAINV, N)
      INITN = N
      ORES = N
C
C Iterate to find result
C
      DO 100 ITER = 1, NITER
      OLDN = N
C
C Take damped step using approximate Newton-Raphson method. 
C
C Bug in CONVEX fc requires SCALAR directive here
C
#include	"cdirscalar.h"
      DO 230 IA1 = 1, NANT
#include	"cdirscalar.h"
         DO 220 IA2 = 1, NANT
#include	"cdirscalar.h"
            DO 210 IA3 = 1, NANT
               IF (TRPWT (IA1,IA2,IA3).GT.0.0) THEN
                  TRPAERR = TRPAINV(IA1,IA2) * TRPAINV(IA2, IA3) *
     1               CONJG(TRPAINV(IA1,IA3)) - TRPARRY(IA1,IA2,IA3)
C
                  DAINV(IA1,IA2) = DAINV(IA1,IA2) + TRPAERR *
     1               CONJG(TRPAINV(IA2,IA3)) * TRPAINV(IA1,IA3)
     2               * TRPWT(IA1,IA2,IA3) 
                  DAINV(IA2,IA3) = DAINV(IA2,IA3) + TRPAERR *
     1               CONJG(TRPAINV(IA1,IA2)) * TRPAINV(IA1,IA3)
     2               * TRPWT(IA1,IA2,IA3) 
                  DAINV(IA1,IA3) = DAINV(IA1,IA3) + CONJG(TRPAERR) *
     1               TRPAINV(IA1,IA2) * TRPAINV(IA2,IA3)
     2               * TRPWT(IA1,IA2,IA3) 
               END IF
 210        CONTINUE
 220     CONTINUE
 230  CONTINUE
C
      DO 260 IA1 = 1, NANT
         DO 250 IA2 = 1, NANT
            DO 240 IA3 = 1, NANT
               IF (TRPWT (IA1,IA2,IA3).GT.0.0) THEN
                  HAINV(IA1,IA2) = HAINV(IA1,IA2) + 
     1               ABS(TRPAINV(IA2,IA3) * TRPAINV(IA1,IA3))**2 
     2               * TRPWT(IA1,IA2,IA3) 
                  HAINV(IA2,IA3) = HAINV(IA2,IA3) + 
     1               ABS(TRPAINV(IA1,IA2) * TRPAINV(IA1,IA3))**2 
     2               * TRPWT(IA1,IA2,IA3) 
                  HAINV(IA1,IA3) = HAINV(IA1,IA3) + 
     1               ABS(TRPAINV(IA1,IA2) * TRPAINV(IA2,IA3))**2 
     2               * TRPWT(IA1,IA2,IA3) 
               END IF
 240        CONTINUE
 250     CONTINUE
 260  CONTINUE
C
      SUMWT = 0.0
      IF (MODE.EQ.'AMPPHI') THEN
         DO 290 IA2 = 1, NANT
            DO 280 IA1 = 1, NANT
               TTRPAINV(IA1,IA2) = TRPAINV(IA1,IA2)
               IF (HAINV(IA1,IA2).NE.0.0) THEN
                  TRPAINV(IA1,IA2) = TRPAINV(IA1,IA2) -
     &               GAIN * DAINV(IA1,IA2)/HAINV(IA1,IA2)
                  TRPAWT (IA1,IA2) = HAINV(IA1,IA2)
               ELSE
                  TRPAWT(IA1,IA2) = 0.0
               END IF
               SUMWT = SUMWT + TRPAWT(IA1,IA2)
 280        CONTINUE
 290     CONTINUE
      ELSE
         DO 291 IA2 = 1, NANT
            DO 281 IA1 = 1, NANT
               TTRPAINV(IA1,IA2) = TRPAINV(IA1,IA2)
               IF (HAINV(IA1,IA2).NE.0.0) THEN
                  CSTEP = TRPAINV(IA1,IA2) -
     &               GAIN * DAINV(IA1,IA2)/HAINV(IA1,IA2)
                  TRPAINV(IA1,IA2) = ABS(TRPAINV(IA1,IA2)) * CSTEP
     $               / ABS(CSTEP)
                  TRPAWT (IA1,IA2) = HAINV(IA1,IA2)
               ELSE
                  TRPAWT(IA1,IA2) = 0.0
               END IF
               SUMWT = SUMWT + TRPAWT(IA1,IA2)
 281        CONTINUE
 291     CONTINUE
      END IF
C
      IF (SUMWT.EQ.0.0) THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE, 'Step is zero')
         GO TO 999
      END IF
C
      CALL TRPCALN (NANT, TRPARRY, TRPWT, TRPAINV, N)
C
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1100) ITER, N
 1100    FORMAT (I4,' N = ', 1PE12.3)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C If we went uphill then try again but with gain reduced unless
C gain is too small if which case we should give up.
C
      IF (N.GT.OLDN) THEN
         IF (SYSDEBUG) THEN
            CALL MSGPUT ('Went uphill - decrease gain and try again', 
     1         'W')
         END IF
         IF (GAIN.LT.0.01) THEN
            GOTO 500
         ELSE
            GAIN = GAIN / 2.0
            GO TO 100
         END IF
      ELSE
         DO 360 IA2 = 1, NANT
            DO 350 IA1 = 1, NANT
               DAINV(IA1,IA2) = 0.0
               HAINV(IA1,IA2) = 0.0
 350        CONTINUE
 360     CONTINUE
      END IF
C
C If good solution then stop.
C
      IF ((N.EQ.0.0).OR.((ABS(N-OLDN).LT.EPS*INITN).AND.
     $   (ITER.GT.1))) GO TO 400
C
  100 CONTINUE
C
C If we got here then the fit did not converge
C
  500 CONTINUE
      CALL ERRREPOR (ERRNTFND, ROUTINE, 'No convergence')
C
C Flag all the data
C
      DO 380 IA2 = 1, NANT
         DO 370 IA1 = 1, NANT
            TRPAWT(IA1,IA2) = 0.0
 370     CONTINUE
 380  CONTINUE
C
C If we jumped directly here then the fit did converge
C
  400 CONTINUE
C
      NRES = N
C
 999  CONTINUE
      END
