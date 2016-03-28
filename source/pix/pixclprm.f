C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixclprm.f	1.1	 3/12/91
C
      SUBROUTINE PIXCLPRM (IN, NIN, OUT, NOUT, CMIN, CMAX)
C
CD We write out an array in which all values below CMIN or above CMAX are removed
C
C       IN      REAL    in      input array
C       NIN     INT     in      size of array
C       OUT     REAL    out      output array
C       NOUT    INT     out      size of array
C       CMIN    REAL    in       min value
C       CMax    REAL    in       max value
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 12 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL              IN(*), OUT(*), CMIN, CMAX
      INTEGER           NIN, NOUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXCLPRM')
C
      INTEGER           I, J
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NOUT = 0
      DO 100 I = 1, NIN
         IF (IN(I) .GE. CMIN .AND. IN(I) .LE. CMAX) THEN
            NOUT = NOUT + 1
            OUT(NOUT) = IN(I)
         ENDIF
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
