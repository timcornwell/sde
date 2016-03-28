C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C	@(#)pixrxexp.f	1.1	 7/1/91
C
      SUBROUTINE PIXRXEXP (IN, FACT, OUT, NT)
C
CD Comlex Pixel level Exponentiation:  OUT(i) = EXP(FACT * IN(i))
C
C	IN	R	input	Input array
C	FACT	X	input	scaling factor
C	OUT	X	output	Output array
C	NT	INT	input	Size of arrays
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 28 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL		IN(*)
      COMPLEX		OUT(*), FACT
      INTEGER		NT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXXEXP')
C
      INTEGER		I
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 100 I = 1, NT
            OUT(I) = EXP (FACT * IN(I))
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
