C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrvarp.f	1.2    11/7/90
C
      SUBROUTINE VISRVARP (VIS, BASE, WT, NVIS, NANT, RVAR)
C
CD Return dispersion in the visibility
C
C
C	VIS	CMPLX	input	Input visibilities
C	BASE	REAL(*)	input	Baselines
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	NANT	INT	input	Number of antennas
C	RVAR	REAL(*,*)	output	Reciprocal of variance in vis
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NANT
      COMPLEX		VIS(NVIS)
      REAL		BASE(NVIS), WT(NVIS)
      REAL		RVAR(NVIS)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRVARP')
C
      INTEGER		IVIS, NCOR, IA1, IA2
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 28)
      COMPLEX		CVIS(MAXNANT,MAXNANT)
      REAL		RERR(MAXNANT, MAXNANT)
      REAL		IERR(MAXNANT, MAXNANT)
      INTEGER		NSUM(MAXNANT,MAXNANT)
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 IA2 = 1, NANT
         DO 11 IA1 = 1, NANT
            CVIS(IA1, IA2) = CMPLX(0.0,0.0)
            RERR(IA1, IA2) = 0.0
            RERR(IA1, IA2) = 0.0
            NSUM(IA1, IA2) = 0
  11     CONTINUE
  10  CONTINUE
C
      NCOR = 0
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         IA1 = NINT(BASE(IVIS)/256.0)
         IA2 = NINT(BASE(IVIS)-FLOAT(256*IA1))
         CVIS(IA1, IA2) = CVIS(IA1, IA2) + VIS(IVIS)
         RERR(IA1, IA2) = RERR(IA1, IA2) + REAL(VIS(IVIS))**2
         IERR(IA1, IA2) = IERR(IA1, IA2) + AIMAG(VIS(IVIS))**2
         NSUM(IA1, IA2) = NSUM(IA1, IA2) + 1
         NCOR = NCOR + 1
 100  CONTINUE
 110  CONTINUE
C
      IF (NCOR.LT.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No unflagged data')
         GO TO 999
      END IF
C
      DO 200 IA2 = 1, NANT
         DO 210 IA1= 1, NANT
            IF (NSUM(IA1, IA2).GT.1) THEN
               CVIS(IA1, IA2) = CVIS(IA1, IA2) / NSUM(IA1, IA2)
               RERR (IA1, IA2) = RERR (IA1, IA2) / NSUM(IA1, IA2)
               IERR (IA1, IA2) = IERR (IA1, IA2) / NSUM(IA1, IA2)
               RERR (IA1, IA2) = SQRT((RERR(IA1, IA2) - 
     1            REAL(CVIS(IA1, IA2))**2)/(NSUM(IA1, IA2)-1))
               IERR (IA1, IA2) = SQRT((IERR(IA1, IA2) - 
     1           AIMAG(CVIS(IA1, IA2))**2)/(NSUM(IA1, IA2)-1))
            END IF
 210     CONTINUE
 200  CONTINUE
C
C Now fill in array
C
      DO 300 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) THEN
            RVAR(IVIS) = 0.0
         ELSE
            IA1 = NINT(BASE(IVIS)/256.0)
            IA2 = NINT(BASE(IVIS)-FLOAT(256*IA1))
            RVAR(IVIS) = 1/(RERR(IA1, IA2)**2 + IERR(IA1, IA2)**2)
         END IF
 300  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
