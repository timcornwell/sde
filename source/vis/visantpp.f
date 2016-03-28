C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visantpp.f	1.4 1/12/95
C
      SUBROUTINE VISANTPP (BASL, TIME, TGAIN, WT, U, V, NVIS, 
     1   NANT, TINT, NUMINT, UPOS, VPOS) 
C
CD Find antenna positions by least-squares fit to the u,v coordinates
C
C
C	BASL	REAL(*)	input	Baselines
C	TIME	REAL(*)	input	Times
C	U,V	REAL(*)	input	U,V coordinates
C	NVIS	INT	input	Number of visibilities
C	NANT	INT	input	Number of antennas
C	TINT	REAL	input	Integration time
C	NUMINT	INT	input	Number of integrations
C	UPOS	REAL(*)	output	Positions of antennas
C	VPOS	REAL(*)	output	Positions of antennas
C Audit trail:
C	Renamed from VISANTPOPIX
C				T.J.Cornwell	Jan 13 1989
C
C	Comment out unreachable statement; elim unused variable.
C				M. Stupar	Dec 28 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NANT, NUMINT
      REAL		BASL(*), TIME(*), U(*), V(*), TGAIN(*)
      REAL		TINT, WT(*)
      REAL		UPOS(NANT, *), VPOS(NANT, *)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISANTPP')
C
      CHARACTER*12	STRTIMC
      INTEGER		IVIS, IA1, IA2, INDEX, ISTART, IEND
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 28)
      REAL		CORU(MAXNANT,MAXNANT), CORV(MAXNANT,MAXNANT)
      INTEGER		NSUM(MAXNANT,MAXNANT)
      REAL		RESID
      INTEGER		NGOOD
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C ********************** Start of loop over integrations *************
C
      ISTART = 1
      DO 1 INDEX = 1, NUMINT
C
C Initialize positions
C
         DO 5 IA1 = 1, NANT
            UPOS(IA1,INDEX) = 0.0
            VPOS(IA1,INDEX) = 0.0
  5      CONTINUE
C
C Initialize accumulators
C
         DO 10 IA2 = 1, NANT
            DO 11 IA1 = 1, NANT
               CORU(IA1, IA2) = 0.0
               CORV(IA1, IA2) = 0.0
               NSUM(IA1, IA2) = 0
  11        CONTINUE
  10     CONTINUE
C
C Loop over data for this integration
C
         NGOOD = 0
         DO 100 IVIS = ISTART, NVIS
            IF (WT(IVIS).LE.0.0) GO TO 100
            IF (TIME(IVIS).LE.TGAIN(INDEX)+TINT/2.0) THEN
               NGOOD = NGOOD + 1
               IA1 = NINT(BASL(IVIS)/256.0)
               IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
               CORU(IA1, IA2) = CORU(IA1, IA2) + U(IVIS)
               CORV(IA1, IA2) = CORV(IA1, IA2) + V(IVIS)
               NSUM(IA1, IA2) = NSUM(IA1, IA2) + 1
            ELSE
               GO TO 110
C              IEND = IVIS - 1
            END IF
 100     CONTINUE
 110     CONTINUE
C
C If no good data then skip the rest of the steps
C
         IF (NGOOD.NE.0) THEN
C
C Normalize correctly
C
            DO 200 IA2 = 1, NANT
               DO 210 IA1= 1, NANT
                  IF (NSUM(IA1, IA2).GT.0) THEN
                     CORU(IA1, IA2) = CORU(IA1, IA2) / NSUM(IA1, IA2)
                     CORV(IA1, IA2) = CORV(IA1, IA2) / NSUM(IA1, IA2)
                  END IF
 210           CONTINUE
 200        CONTINUE
C
C Do solution for this set of data
C
            CALL CALANTPO (CORU, NSUM, NANT, UPOS(1,INDEX), RESID)
            CALL CALANTPO (CORV, NSUM, NANT, VPOS(1,INDEX), RESID)
C
         ELSE
            MESSAGE = 'Cannot find antenna locations for time: '
            CALL STRAPPEN (MESSAGE, STRTIMC(TGAIN(INDEX)))
            CALL MSGPUT (MESSAGE, 'W')
         END IF
C
         ISTART = MIN(IEND + 1, NVIS)
C
  1   CONTINUE
C
C ********************** End of loop over integrations *****************
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
