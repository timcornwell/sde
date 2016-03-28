C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdetp.f	1.1 8/20/92
C
      SUBROUTINE VISDETP (VIS, WT, MVIS, MWT, NVIS, AMP, SUMWT, CHISQ)
C
CD Find fit to data
C
C	VIS	CMPLX	input	Input visibilities
C	MVIS	CMPLX	input	Input model visibilities
C	WT	REAL(*)	input	Input weights
C	MWT	REAL(*)	input	Input model weights
C Audit trail:
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS
      REAL		WT(*), MWT(*), AMP, SUMWT, CHISQ
      COMPLEX		VIS(*), MVIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDETP')
C
      INTEGER		IVIS
      REAL		TOP, BOT
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      TOP = 0.0
      BOT = 0.0
      DO 90 IVIS = 1, NVIS
         IF ((WT(IVIS).LE.0.0).AND.(MWT(IVIS).LE.0.0)) GO TO 90
            TOP = TOP + ABS(VIS(IVIS)*CONJG(MVIS(IVIS)))*WT(IVIS)
            BOT = BOT + ABS(MVIS(IVIS))**2*WT(IVIS)
  90  CONTINUE
C
      IF(BOT.NE.0.0) THEN
         AMP = TOP/BOT
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No data weight')
         GO TO 999
      END IF
C
      SUMWT = 0.0
      CHISQ = 0.0
      DO 100 IVIS = 1, NVIS
         IF ((WT(IVIS).LE.0.0).AND.(MWT(IVIS).LE.0.0)) GO TO 100
            SUMWT = SUMWT + WT(IVIS)
            CHISQ = CHISQ + ABS(VIS(IVIS)-AMP*MVIS(IVIS))**2*WT(IVIS)
 100     CONTINUE
C
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
