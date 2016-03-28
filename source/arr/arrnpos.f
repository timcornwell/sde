C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrnpos.f	1.1    7/13/94
C
      INTEGER FUNCTION ARRNPOS (A)
C
CD Return the number of positive pixels in an array
C
C	A	CH*(*)	input	Name of array
C
C Audit trail:
C	Original version:
C				D.S.Briggs	July 8 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRNPOS')
C
      INTEGER		NP
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL ARRCLIP (A, 0.0, 1.0, 'TMP-ARRNPOS')
      CALL ARRDIV ('TMP-ARRNPOS', 'TMP-ARRNPOS', 'TMP-ARRNPOS')
      CALL ARRNSPRT ('TMP-ARRNPOS', NP)
      CALL DATDELET ('TMP-ARRNPOS')
C
      ARRNPOS = NP
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
