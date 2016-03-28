C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissigmp.f	1.1 8/20/92
C
      SUBROUTINE VISSIGMP (VIS, WT, NVIS, NGOOD, SUMWT, CHISQ)
C
CD Find sigma of data
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
      INTEGER		NVIS, NGOOD
      REAL		WT(*), SUMWT, CHISQ
      COMPLEX		VIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISEDITP')
C
      INTEGER		IVIS
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      SUMWT = 0.0
      CHISQ = 0.0
      NGOOD = 0
C
      DO 90 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 90
            NGOOD = NGOOD + 1
            SUMWT = SUMWT + WT(IVIS)
            CHISQ = CHISQ + ABS(VIS(IVIS))**2*WT(IVIS)            
  90  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
