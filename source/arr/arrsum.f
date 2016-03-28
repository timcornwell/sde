C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsum.f	1.2    6/7/93
C
      SUBROUTINE ARRSUM (A1, SUM)
C
CD Sum the elements in the array
C
C Arguments: CALL ARRSUM (A1, SUM)
C
C	A1	CH*(*)	input	Name of array
C	SUM	REAL	output	SUM
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 12 1991
C	Double precision capability added
C				D.S.Briggs	March 25 1993
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1
      REAL		SUM
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSUM')
C
      CHARACTER*1	T1
      INTEGER		I, N1, NAXIS1(SYSMXDIM)
      INTEGER		ADD1, NT
      LOGICAL		DATEXIST
      DATA		NAXIS1	/SYSMXDIM * 1/
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      NT = 1
      DO 10 I = 1, N1
            NT = NT * NAXIS1(I)
  10  CONTINUE
C
C Call appropriate routine
C
      IF (T1.EQ.'R') THEN
         CALL PIXRSUM (MEMR(ADD1), NT, SUM)
      ELSE IF (T1.EQ.'D') THEN
         CALL PIXDSUM (MEMD(ADD1), NT, SUM)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      ' Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
