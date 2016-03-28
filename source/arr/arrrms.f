C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrrms.f	1.3    11/7/90
C
      SUBROUTINE ARRRMS (IN, RMS, SUMSQ)
C
CD Form rms
C
C
C	IN	CH*(*)	input	Name of array
C	RMS	REAL	output	RMS
C	SUMSQ	REAL	output 	Sum of squares
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		RMS, SUMSQ
      CHARACTER*(*)	IN
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRRMS')
C
      CHARACTER*1	T1
      INTEGER		I, N1, NAXIS1(SYSMXDIM)
      INTEGER		ADD1, NT, NDUMMY
C
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, N1, NAXIS1, T1, ADD1)
      NT = 1
      DO 10 I = 1, N1
         NT = NT * NAXIS1(I)
  10  CONTINUE 
C
      IF (T1.EQ.'R') THEN
         CALL PIXRRMS (MEMR(ADD1), NT, RMS)
         SUMSQ = NT * RMS**2
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
C
      IF (RMS.LT.0.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Chisquared < 0.0')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
