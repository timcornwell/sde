C
C       National Radio Astronomy Observatory, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)utllint.f	1.1    2/2/94
C
      INTEGER FUNCTION UTLLINT(F)
C
CD    Least integer function (ceiling)
C
C	UTLGINT	INT	output	the least integer greater than or equal to F
C	F	REAL	input	defined above
C
C Audit trail:
C				D.S.Briggs	Jan 31 1994
C----------------------------------------------------------------------
#include	"stdinc.h"
C	
      REAL	F
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLLINT')
C
      INTEGER		LINT
C=======================================================================
C						positive sign is annoying
      IF (F .GE. 0) THEN
         IF (REAL(INT(F)).EQ.F) THEN
            LINT = INT(F)
         ELSE
            LINT = INT(F) + 1
         END IF
      ELSE
         LINT = INT(F)
      ENDIF
C
      UTLLINT = LINT
C
      RETURN
      END
