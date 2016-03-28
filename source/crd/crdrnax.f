C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdrnax.f	1.3    11/7/90
C
      INTEGER FUNCTION CRDRNAX (NAX, NAXIS)
C
CD Find number of real axes
C
C
C	NAX	INT	input	Number of axes
C	NAXIS	INT(*)	input	Array of axes
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NAX, NAXIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDRNAX')
C
      INTEGER		IAX, TNAX
C=====================================================================
      TNAX = 0
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 IAX = 1, NAX
         IF (NAXIS(IAX).GT.1) THEN
            TNAX = IAX
         END IF
  10  CONTINUE
C
  999 CONTINUE
      CRDRNAX = TNAX
      END
