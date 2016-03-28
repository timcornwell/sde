C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visblcop.f	1.2 22 Feb 1995
C
      SUBROUTINE VISBLCOP (VIS, BASE, WT, NVIS, NANT, CORGAIN,
     1   NEWVIS, NEWWT, NFLAG)
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
      COMPLEX		VIS(NVIS), NEWVIS(NVIS), CORGAIN(NANT, *)
      REAL		BASE(NVIS), WT(NVIS)
      REAL		NEWWT(NVIS)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISBLCOP')
C
      INTEGER		IVIS, IA1, IA2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NFLAG = 0
      DO 300 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) THEN
            NEWVIS(IVIS) = VIS(IVIS)
            NEWWT(IVIS) = WT(IVIS)
            NFLAG = NFLAG + 1
            GO TO 300
         END IF
         IA1 = NINT(BASE(IVIS)/256.0)
         IA2 = NINT(BASE(IVIS)-FLOAT(256*IA1))
         IF (ABS(CORGAIN(IA1,IA2)).NE.0.0) THEN
            NEWVIS(IVIS) = VIS(IVIS) / CORGAIN (IA1,IA2)
         ELSE
            NEWVIS(IVIS) = VIS(IVIS)
            NEWWT(IVIS) = SIGN (WT(IVIS), -1.0)
            NFLAG = NFLAG + 1
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
