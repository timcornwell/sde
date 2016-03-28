C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrpread.f	1.1 12/23/92
C
      SUBROUTINE ARRPREAD (A1)
C
CD READ an array from PVM
C
C Audit trail:
C
C	Original version.
C
C	Update pvm2.4 -> pvm3.3.
C					M. Stupar	Mar 05, 1995
C--------------------------------------------------------------------
#include	"stdinc.h"
#include	"fpvm3.h"
C
      CHARACTER*(*)	A1
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRPREAD')
C
      CHARACTER*1	T1, TYPES(3)
      INTEGER		I, N1, RN1, NAXIS1(SYSMXDIM)
      INTEGER		ADD1, NT, INFO
C
      INTEGER		CRDRNAX
      LOGICAL		DATEXIST
      DATA		TYPES /'R', 'X', 'I'/
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL PVMFUNPACK (INTEGER4, N1, 1, INFO)
      CALL PVMFUNPACK (INTEGER4, NAXIS1, N1, INFO)
      CALL PVMFUNPACK (INTEGER4, I, 1, INFO)
      T1=TYPES(I)
C
      IF(.NOT.DATEXIST(A1)) THEN
         CALL DATMAKAR (A1, N1, NAXIS1, T1, ADD1)
      ELSE
         CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      END IF
      IF (ERROR) GO TO 990
      RN1 = CRDRNAX (N1, NAXIS1)
      NT = 1
      DO 10 I = 1, RN1
         NAXIS1(I) = MAX(1, NAXIS1(I))
         NT = NT * NAXIS1(I)
  10  CONTINUE
C
C Call appropriate routine
C
      IF (T1.EQ.'R') THEN
         CALL PVMFUNPACK (REAL4, MEMR(ADD1), NT, INFO)
      ELSE IF (T1.EQ.'X') THEN
         CALL PVMFUNPACK (COMPLEX8, MEMX(ADD1), NT, INFO)
      ELSE IF (T1.EQ.'I') THEN
         CALL PVMFUNPACK (INTEGER4, MEMI(ADD1), NT, INFO)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      ' Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
