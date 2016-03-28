C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgimag.f	1.2    3/26/91
C
      SUBROUTINE IMGIMAG (X, I)
C
CD Take the IMAG part of a complex image
C
C	X	CH*(*)	input	Complex Image
C	I	CH*(*)	input	IMAGINARY Image
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
      CHARACTER*(*)	X, I
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGIMAG')
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL ARRIMAG (X, I)
      CALL HEDCOPY (X, I)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
