C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrnpix.f	1.1    6/7/93
C
      INTEGER FUNCTION ARRNPIX (A)
C
CD Return the number of pixels in an array
C
C	A	CH*(*)	input	Name of array
C
C Audit trail:
C	Original version:
C				D.S.Briggs	May 5 1993
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRNPIX')
C
      CHARACTER*1	T
      INTEGER		I, NAX, NAXIS(SYSMXDIM), NT, ADD
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, NAX, NAXIS, T, ADD)
      NT = 1
      DO 10 I = 1, NAX
         NAXIS(I) = MAX(1, NAXIS(I))
         NT = NT * NAXIS(I)
  10  CONTINUE
C
      ARRNPIX = NT
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
