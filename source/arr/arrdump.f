C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrdump.f	1.2    7/18/97
C
      SUBROUTINE ARRDUMP (A)
C
CD Dump array information
C
C	A	CH*(*)	input	Name of array
C
C Audit trail:
C	Original version:
C				D.S.Briggs	22 July 1993
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRDUMP')
C
      CHARACTER*1	ATYPE
      CHARACTER*12	CHADD
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD, NT, I
C
      CHARACTER*12	STRINT
      INTEGER		STRLEN
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF (DATEXIST(A)) THEN
         CALL DATGETAR (A, NAX, NAXIS, ATYPE, ADD)
         CHADD = STRINT (ADD)
         MESSAGE = 'Array ' // A(1:STRLEN(A)) // ' at address '
     $      // CHADD(1:STRLEN(CHADD)) // ' has type ' // ATYPE
         CALL MSGPUT (MESSAGE, 'L')
         WRITE (MESSAGE, 1000) NAX
 1000    FORMAT ('NAX =',I2)
         CALL MSGPUT (MESSAGE, 'L')
         NT = 1
         DO 100 I = 1, NAX
            WRITE (MESSAGE, 1010) I, NAXIS(I)
 1010       FORMAT ('NAXIS(',I1,') =',I6)
            CALL MSGPUT (MESSAGE, 'L')
            NT = NT * NAXIS(I)
 100     CONTINUE
         WRITE (MESSAGE, 1100) NT
 1100    FORMAT (I8,' total elements')
         CALL MSGPUT (MESSAGE, 'L')
      ELSE
         CALL MSGPUT ('Warning: Array ''' // A(1:STRLEN(A)) //
     $      ''' does not exist', 'W')
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
