C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arreq.f	1.1    12/11/92
C
      LOGICAL FUNCTION ARREQ (A1, A2)
C
CD Return true if arrays are exactly equal
C
C	A1	CH*(*)	input	Name of array
C	A2	CH*(*)	input	Name of array
C
C Audit trail:
C	Original version
C				D.S.Briggs	Nov 30 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARREQ')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM)
      INTEGER		ADD1, ADD2, NT
C
      LOGICAL		PIXREQ, PIXDEQ, PIXXEQ
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      CALL DATGETAR (A2, N2, NAXIS2, T2, ADD2)
      NT = 1
      DO 10 I = 1, MAX(N1, N2)
         IF (NAXIS1(I).NE.NAXIS2(I)) THEN
            ARREQ = .FALSE.
            GO TO 999
         ELSE
            NT = NT * NAXIS1(I)
         END IF
  10  CONTINUE
C
      IF (T1.NE.T2) THEN
         ARREQ = .FALSE.
         GO TO 999
      END IF
C
C Call appropriate routine
C
      IF (T1.EQ.'R') THEN
         ARREQ = PIXREQ (MEMR(ADD1), MEMR(ADD2), NT)
      ELSE IF (T1.EQ.'D') THEN
         ARREQ = PIXDEQ (MEMD(ADD1), MEMD(ADD1), NT)
      ELSE IF (T1.EQ.'X') THEN
         ARREQ = PIXXEQ (MEMX(ADD1), MEMX(ADD1), NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
