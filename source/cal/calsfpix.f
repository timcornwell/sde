C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calsfpix.f	1.3    11/7/90
C
      SUBROUTINE CALSFPIX (ANTGAIN, NANT, NUMINT, SF, NUM, NLAG,
     1   SFTYPE)
C
CD Find calibration structure function
C
C
C	ANTGAIN	CMPLX	input	Antenna gains
C	NANT	INT	input	Number of antennas
C	NUMINT	INT	input	Number of integrations
C	SF	CMPLX	output	Structure function
C	NUM	INT	output	Number of points
C	NLAG	INT	input	Number of correlations
C	SFTYPE	CH*(*)	input	Type of structure function
C				'AMP'|'PHASE'|'AMPPHASE'
C Audit trail:
C	Changed to ignore even/odd distinction
C				T.J.Cornwell	Jan 11 1989
C	Changed to add SFTYPE
C				T.J. Cornwell	Jan 12 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NANT, NUMINT, NLAG
      COMPLEX		ANTGAIN(NANT,*)
      REAL		SF(NANT, NANT, *)
      INTEGER		NUM (NANT, NANT, *)
      CHARACTER*(*)	SFTYPE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CALSFPIX')
C
      INTEGER		IA1, IA2, ILAG, INTNDX
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Apply initial calibrations
C
      DO 13 INTNDX = 2, NUMINT
         DO 12 IA1 = 1, NANT
            IF (ABS(ANTGAIN(IA1,1)).NE.0.0) THEN
               ANTGAIN(IA1,INTNDX) = ANTGAIN(IA1,INTNDX) /
     1            ANTGAIN(IA1,1)
            ELSE
               ANTGAIN(IA1,INTNDX) = 0.0
            END IF
  12     CONTINUE
  13  CONTINUE
C
C Now fix up antenna gains as required: note that this destroys the
C original values
C
      IF (SFTYPE.EQ.'AMPPHASE') THEN
      ELSE IF (SFTYPE.EQ.'PHASE') THEN
         DO 3 IA1 = 1, NANT
            DO 4 INTNDX = 1, NUMINT
               IF(ABS(ANTGAIN(IA1,INTNDX)).GT.0.0) THEN
                  ANTGAIN(IA1,INTNDX) = ANTGAIN(IA1,INTNDX) 
     1               / ABS(ANTGAIN(IA1,INTNDX))
               END IF
  4         CONTINUE
  3      CONTINUE
      ELSE IF (SFTYPE.EQ.'AMP') THEN
         DO 5 IA1 = 1, NANT
            DO 6 INTNDX = 1, NUMINT
               ANTGAIN(IA1,INTNDX) = ABS(ANTGAIN(IA1,INTNDX))
  6         CONTINUE
  5      CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unknown SFTYPE ')
         GO TO 999
      END IF
C
      DO 30 ILAG = 1, NLAG
         DO 20 IA2 = 1, NANT
            DO 10 IA1 = 1, NANT
               SF(IA1, IA2, ILAG) = 0.0
               NUM(IA1, IA2, ILAG) = 0
  10        CONTINUE
  20     CONTINUE
  30  CONTINUE
C
C Find SF
C
      DO 70 ILAG = 1, NLAG
         DO 60 INTNDX = 1, MIN(NUMINT-(ILAG-1), NUMINT)
            DO 50 IA2 = 1, NANT
               IF (ABS(ANTGAIN(IA2,INTNDX)).GT.0.0) THEN
                  DO 40 IA1 = 1, NANT
                     IF (ABS(ANTGAIN(IA1,INTNDX)).GT.0.0) THEN
                        NUM(IA1,IA2,ILAG) = NUM(IA1,IA2,ILAG) + 1
                        SF(IA1,IA2,ILAG) = SF(IA1,IA2,ILAG) +
     1                     0.5 * ABS(ANTGAIN(IA1,INTNDX) -
     2                     ANTGAIN(IA2,INTNDX+(ILAG-1)))**2
                     END IF
  40              CONTINUE
               END IF
  50        CONTINUE
  60     CONTINUE
  70  CONTINUE
C
      DO 200 ILAG = 1, NLAG
         DO 190 IA2 = 1, NANT
            DO 180 IA1 = 1, NANT
               IF(NUM(IA1,IA2,ILAG).GT.0) THEN
                  SF(IA1,IA2,ILAG) = SF(IA1,IA2,ILAG) /
     1               NUM(IA1,IA2,ILAG)
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

