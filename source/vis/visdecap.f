C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdecap.f	1.5 06 Feb 1995
C
      SUBROUTINE VISDECAP (VIS, BASE, WT, MVIS, MWT, NVIS,
     $   NANT, DERR, A2)
C
CD Calculate Delay error
C
C
C	VIS	CMPLX	input	Input visibilities
C	BASE	REAL(*)	input	Baselines
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	NANT	INT	input	Number of antennas
C	FLUX	REAL	input	Estimated flux
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 25 1995
C	Changed solution by factor of two.
C				T.J.Cornwell	Jan 25 1995
C	Changed solution by factor of 1/2!
C				T.J.Cornwell	Feb 6 1995
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NANT
      COMPLEX		VIS(NVIS), MVIS(NVIS)
      REAL		BASE(NVIS), WT(NVIS), MWT(NVIS)
      REAL		DERR(*), A2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDECAP')
C
      INTEGER		IVIS, IA1, IA2
      REAL		TOP, BOT
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      TOP = 0.0
      BOT = 0.0
      DO 300 IVIS = 1, NVIS
         IF ((WT(IVIS).GT.0.0).AND.(MWT(IVIS).GT.0.0)) THEN
            IA1 = NINT(BASE(IVIS)/256.0)
            IA2 = NINT(BASE(IVIS)-FLOAT(256*IA1))
            TOP = TOP +
     $         REAL((VIS(IVIS)-MVIS(IVIS))*CONJG(MVIS(IVIS)))
     $         * WT(IVIS) * (2.0*DERR(IA1)*DERR(IA2))
            BOT = BOT + ABS(CONJG(MVIS(IVIS)))**2
     $         * WT(IVIS) * (2.0*DERR(IA1)*DERR(IA2))**2
         END IF
 300  CONTINUE
C
      A2 = TOP/BOT
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
