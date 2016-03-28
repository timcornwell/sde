C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visnormp.f	1.1 9/29/92
C
      SUBROUTINE VISNORMP (VIS, MVIS, WT, MWT, NVIS, FACTOR)
C
CD Find normalization factor OBS/MODEL
C
C	VIS	CMPLX	input	Input visibilities
C	MVIS	CMPLX	input	Input model visibilities
C	WT	REAL(*)	input	Input weights
C	MWT	REAL(*)	input	Input model weights
C	NVIS	INT	input	Number of visibilities
C	FACTOR	REAL	output	OBS/MODEL
C Audit trail:
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS
      REAL		WT(*), MWT(*)
      REAL		FACTOR
      COMPLEX		VIS(*), MVIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISNORMP')
C
      INTEGER		IVIS
      REAL		TOP, BOTTOM
C=====================================================================
      FACTOR = 1.0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      TOP = 0.0
      BOTTOM = 0.0
      DO 90 IVIS = 1, NVIS
         IF ((WT(IVIS).LE.0.0).OR.(MWT(IVIS).LE.0.0).OR.
     1      (ABS(MVIS(IVIS)).EQ.0.0)) GO TO 90
         TOP = TOP + ABS(VIS(IVIS))*ABS(MVIS(IVIS))*MWT(IVIS)
         BOTTOM = BOTTOM + ABS(MVIS(IVIS))**2*MWT(IVIS)
  90  CONTINUE
C
      IF(BOTTOM.NE.0.0) THEN
         FACTOR = TOP/BOTTOM
      ELSE
         FACTOR = 1.0
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No valid data')
         GO TO 999
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
