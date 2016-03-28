C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrcentr.f	1.1	 5/1/95
C
      SUBROUTINE ARRCENTR (ARR, CENTX, CENTY)
C
CD Find the centroid of an array
C
C	ARR	CH*(*)	input	Name of array
C	CENTX	REAL	input	Pix centroid
C	CENTY	REAL	input	Pix centroid
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	May 1 1995
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ARR
      REAL		CENTX, CENTY
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRABS')
C
      CHARACTER*1	T
      INTEGER		I, N, NAXIS(SYSMXDIM)
      INTEGER		ADD
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (ARR, N, NAXIS, T, ADD)
      DO 10 I = 1, N
         NAXIS(I) = MAX(1, NAXIS(I))
  10  CONTINUE
C
C Call appropriate routine
C
      IF (T.EQ.'R' .AND. NAXIS(3) .EQ. 1) THEN
         CALL PIXCENTR (MEMR(ADD), NAXIS(1), NAXIS(2),
     $        CENTX, CENTY)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array type or shape not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
