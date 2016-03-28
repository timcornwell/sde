C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsqrt.f	1.1 5/6/92
C
      SUBROUTINE ARRSQRT (A1, A2)
C
CD Form the sqrt of an array
C
C
C	A1	CH*(*)	input	Name of array
C	A2	CH*(*)	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Feb 10 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSQRT')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM)
      INTEGER		ADD1, ADD2, NT
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      NT = 1
      DO 10 I = 1, N1
         NAXIS1(I) = MAX(1, NAXIS1(I))
         NT = NT * NAXIS1(I)
  10  CONTINUE
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(A2)) THEN
         T2 = 'R'
	 N2 = N1
         DO 20 I = 1, N2
            NAXIS2(I) = NAXIS1(I)
  20     CONTINUE
         CALL DATMAKAR (A2, N2, NAXIS2, T2, ADD2)
      ELSE
         CALL DATGETAR (A2, N2, NAXIS2, T2, ADD2)
         IF (N1.NE.N2) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Different number of axes')
            GO TO 999
         END IF
         DO 30 I = 1,N2
            NAXIS2(I) = MAX(1, NAXIS2(I))
            IF (NAXIS2(I).NE.NAXIS1(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     1            'Axes disagree')
               GO TO 999
            END IF
  30     CONTINUE
      END IF
C
C Call appropriate routine
C
      IF (T1.EQ.'R') THEN
         CALL PIXRSQRT (MEMR(ADD1), MEMR(ADD2), NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
