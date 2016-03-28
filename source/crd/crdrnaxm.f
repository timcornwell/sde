C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdrnaxm.f	1.1    8/4/91
C
      INTEGER FUNCTION CRDRNAXM (NAX, NAXIS)
C
CD Find number of real axes and return them in NAXIS
C
C
C	NAX	INT	input	Number of axes
C	NAXIS	INT(*)	input	Array of axes
C Audit trail:
C	Cloned from crdrnax
C				R.G. Marson 19 June, 1991
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NAX, NAXIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDRNAXM')
C
      INTEGER		IAX, TNAX
C=====================================================================
      TNAX = 0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      TNAX = 0
      DO 10 IAX = 1, NAX
         IF (NAXIS(IAX).GT.1) THEN
            TNAX = TNAX + 1
            NAXIS(TNAX) = NAXIS(IAX)
         END IF
  10  CONTINUE
C
  999 CONTINUE
      CRDRNAXM = TNAX
      END
