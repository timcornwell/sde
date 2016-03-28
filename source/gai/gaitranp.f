C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gaitranp.f	1.1    7/13/94
C
      SUBROUTINE GAITRANP (VISIN, WTIN, VISEX, WTEX, VISREF, WTREF,
     $   VISOUT, WTOUT, NVIS, MODE)
C
CD Tranfer the calibration from one (pair) of vis sets to another.
C
C     VISIN	X(NVIS)	input	Input data set
C     WTIN	R(NVIS) input
C     VISEX	X(NVIS)	input	Example data set (cal to be transferred)
C     WTEX	R(NVIS)	input
C     VISREF	X(NVIS)	input	Reference data set  (model or good cal)
C     WTREF	R(NVIS)	input
C     VISOUT	X(NVIS)	output	Output data set
C     WTOUT	R(NVIS)	output
C     NVIS	INT	input
C     MODE	CH*(*)	input	AMPPHASE, AMP, PHASE
C
C See gaitrans.f for the full documentation.
C
C Audit trail:
C	Original version:
C				D.S.Briggs	July 8 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS
      COMPLEX		VISIN(NVIS), VISOUT(NVIS), VISREF(NVIS),
     $   		VISEX(NVIS)
      REAL		WTIN(NVIS), WTOUT(NVIS), WTREF(NVIS),
     $   		WTEX(NVIS)
      CHARACTER*(*)	MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GAITRANP')
C
      INTEGER		I
C=====================================================================
      IF (ERROR) GO TO 999
C
C Weights first
C      
      DO 100 I = 0, NVIS-1
         IF ((WTIN(I).LE.0.0).OR.(WTREF(I).LE.0.0).OR.
     $      (WTEX(I).LE.0.0)) THEN
            WTOUT(I) = -ABS(WTIN(I))
         ELSE
            WTOUT(I) = WTIN(I)
         END IF
 100  CONTINUE
C
      IF (MODE.EQ.'AMPPHASE') THEN
         DO 200 I = 0, NVIS-1
            IF (WTOUT(I).GT.0.0) THEN
               VISOUT(I) = VISIN(I) * VISEX(I) / VISREF(I)
            ELSE
               VISOUT(I) = (0.0,0.0)
            END IF
 200     CONTINUE
      ELSE IF (MODE.EQ.'AMP') THEN
         DO 300 I = 0, NVIS-1
            IF (WTOUT(I).GT.0.0) THEN
               VISOUT(I) = VISIN(I) * ABS(VISEX(I) / VISREF(I))
            ELSE
               VISOUT(I) = (0.0,0.0)
            END IF
 300     CONTINUE
      ELSE IF (MODE.EQ.'PHASE') THEN
         DO 400 I = 0, NVIS-1
            IF (WTOUT(I).GT.0.0) THEN
               VISOUT(I) = VISIN(I) * VISEX(I) / VISREF(I)
     $            / ABS(VISEX(I) / VISREF(I))
            ELSE
               VISOUT(I) = (0.0,0.0)
            END IF
 400     CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unrecognized MODE')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
