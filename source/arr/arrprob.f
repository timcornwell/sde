C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrprob.f	1.4	 7/20/92
C
      SUBROUTINE ARRPROB (DRT, DPRED, PROB)
C
C Form probability
C
C	DRT	CH*(*)	input	Name of array
C	DPRED	CH*(*)	input	Name of array
C	PROB	REAL	output	PROB
C Audit trail:
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		PROB
      CHARACTER*(*)	DRT, DPRED
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRPROB')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM)
      INTEGER		ADD1, ADD2, NT
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (DRT, N1, NAXIS1, T1, ADD1)
      CALL DATGETAR (DPRED, N2, NAXIS2, T2, ADD2)
      IF (N1.NE.N2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Different number of axes')
         GO TO 999
      END IF
      NT = 1
      DO 10 I = 1, N1
         IF (NAXIS1(I).NE.NAXIS2(I)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes do not match')
            GO TO 999
         ELSE
            NT = NT * NAXIS1(I)
         END IF
  10  CONTINUE 
C
      IF (T1.EQ.'R') THEN
         CALL PIXRPROB (MEMR(ADD1), MEMR(ADD2), NT, PROB)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
