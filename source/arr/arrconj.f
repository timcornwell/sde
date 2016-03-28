C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrconj.f	1.3    11/7/90
C
      SUBROUTINE ARRCONJ (A)
C
CD  Complex Conjugate of an array
C
C
C	A	CH*(*)	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRCONJ')
C
      CHARACTER*1	T
      INTEGER		I, N, NAXIS(SYSMXDIM)
      INTEGER		ADD, NT
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, N, NAXIS, T, ADD)
      NT = 1
      DO 10 I = 1, N
         NT = NT * MAX(1, NAXIS(I))
  10  CONTINUE
      IF (NT.EQ.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero pixels')
         GO TO 999
      END IF
C
C Call appropriate routine
C
      IF (T.EQ.'X') THEN
         CALL PIXCONJ (MEMX(ADD), NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T//
     1     'Not supported')
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
