C
C       National Radio Astronomy Observatory, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)utlgint.f	1.2	 7/8/91
C
      INTEGER FUNCTION UTLGINT(F)
C
CD    Greatest integer function
C
C	UTLGINT	INT	output	the greatest integer less than or equal to F
C	F	REAL	input	defined above
C
C Audit trail:
C				M.A.Holdaway	Sep 5 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C	
      REAL	F
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLGINT')
C
      INTEGER		GINT
C=======================================================================
C
      IF (F .GE. 0) THEN
         GINT = INT(F)
      ELSE
C 						negative number
         IF (FLOAT(INT(F)) .EQ. F) THEN
C						GINT(-3.0) = -3
            GINT = INT(F)
         ELSE
C						GINT(-2.4) = -3
            GINT = INT(F) - 1
         ENDIF
      ENDIF
C
      UTLGINT = GINT
C
      RETURN
      END
