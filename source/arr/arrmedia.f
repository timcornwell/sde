C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrmedia.f	1.2	 3/12/91
C
      SUBROUTINE ARRMEDIA (A, CLIPMIN, CLIPMAX, MEDIAN)
C
C Finds the median of ARRAY values greater than CLIPMIN and less
C than CLIPMAX
C
C	A	CH*(*)	IN	Name of array
C	CLIPMIN	REAL	IN	neglect below here
C	CLIPMAX	REAL	IN	neglect above here
C	MEDIAN	REAL	OUT	Returned value of median
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
      CHARACTER*(*)	A
      REAL		CLIPMIN, CLIPMAX, MEDIAN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRMEDIA')
C
      INTEGER		N, NAXIS(SYSMXDIM), ADD, I, NT, ADD2,
     $   		IMIN, IMAX, IMED, NNEW
      CHARACTER*(1)	T
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, N, NAXIS, T, ADD)
      NT = 1
      DO 10 I = 1, N
            NT = NT * NAXIS(I)
  10  CONTINUE
      NAXIS(1) = NT
      NAXIS(2) = 1
      NAXIS(3) = 1
      CALL DATMAKAR ('SortedArray', 1, NAXIS, T, ADD2)
C
      IF (T .EQ. 'R') THEN
         CALL PIXCLPRM (MEMR(ADD), NT, MEMR(ADD2), NNEW, CLIPMIN,
     $      CLIPMAX)
C
         CALL UTLRSORT (NNEW, MEMR(ADD2), MEMR(ADD2) )
C
         IMED = NNEW / 2
         MEDIAN = MEMR(ADD2 + IMED - 1)
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'There is no subroutine to get median of type'//T)
         GOTO 990
      ENDIF
      CALL DATDELET ('SortedArray')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
