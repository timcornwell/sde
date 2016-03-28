C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgreal.f	1.1    3/20/91
C
      SUBROUTINE IMGREAL (X, R)
C
CD Take the real part of a complex image
C
C	X	CH*(*)	input	Complex Image
C	R	CH*(*)	input	Real Image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Feb 27 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	X, R
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGREAL')
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL ARRREAL (X, R)
      CALL HEDCOPY (X, R)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
