C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)viscalpi.f	1.2    11/7/90
C
      SUBROUTINE VISCALPI (VIS, BASE, WT, NVIS, NANT, FLUX,
     1   NEWVIS, NEWWT, NFLAG, RESID)
C
CD Calibrate data
C
C
C	VIS	CMPLX	input	Input visibilities
C	BASE	REAL(*)	input	Baselines
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	NANT	INT	input	Number of antennas
C	FLUX	REAL	input	Estimated flux
C	NEWVIS	CMPLX	output	Output visibilities
C	NEWWT	REAL	output	Output weights
C	NFLAG	INT	output	Number of vis. flagged
C	RESID	REAL	output	Residual
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NANT, NFLAG
      COMPLEX		VIS(NVIS), NEWVIS(NVIS)
      REAL		BASE(NVIS), WT(NVIS)
      REAL		NEWWT(NVIS), RESID, FLUX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCALPI')
C
      INTEGER		IVIS, NCOR, IA1, IA2
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 28)
      COMPLEX		CVIS(MAXNANT,MAXNANT), ANTGAIN(MAXNANT),
     1			CORGAIN
      REAL		CWT(MAXNANT,MAXNANT)
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
            CWT(IA1, IA2) = 0.0
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
         CWT(IA1, IA2) = CWT(IA1, IA2) + WT(IVIS)
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
            IF (NSUM(IA1, IA2).GT.0) THEN
               CVIS(IA1, IA2) = CVIS(IA1, IA2) / (FLUX * NSUM(IA1, IA2))
               CWT(IA1, IA2) = CWT(IA1, IA2) / NSUM(IA1, IA2)
            END IF
 210     CONTINUE
 200  CONTINUE
C
      CALL CALANTSO (CVIS, CWT, NANT, ANTGAIN, RESID)
C
      NFLAG = 0
      DO 300 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 300
         IA1 = NINT(BASE(IVIS)/256.0)
         IA2 = NINT(BASE(IVIS)-FLOAT(256*IA1))
         CORGAIN = ANTGAIN(IA1) * CONJG(ANTGAIN(IA2))
         IF (ABS(CORGAIN).NE.0.0) THEN
            NEWVIS(IVIS) = VIS(IVIS) / CORGAIN
         ELSE
            NEWVIS(IVIS) = VIS(IVIS)
            NEWWT(IVIS) = SIGN (WT(IVIS), -1.0)
            NFLAG = NFLAG + 1
         END IF
 300  CONTINUE
 310  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
