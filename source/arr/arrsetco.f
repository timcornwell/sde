C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsetco.f	1.6    12/11/92
C
      SUBROUTINE ARRSETCO (A, TOTAL, EACH)
C
CD Set an array to a constant
C
C
C	A	CH*(*)	input	Name of array
C	TOTAL	REAL	input	Total power to be distributed
C	EACH	REAL	input	plus EACH per pixel
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added I
C				M.A.Holdaway	June 27 1991		
C	Added D
C				D.S.Briggs	Nov 24 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A
      REAL		TOTAL, EACH
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSETCO')
C
      CHARACTER*1	T
      INTEGER		I, N, NAXIS(SYSMXDIM)
      INTEGER		ADD, NT
      REAL		CONST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, N, NAXIS, T, ADD)
      IF (ERROR) GO TO 990
      NT = 1
      DO 10 I = 1, N
         NT = NT * MAX(1, NAXIS(I))
  10  CONTINUE
C
C Call appropriate routine
C
      CONST = TOTAL/NT + EACH
      IF (T.EQ.'R') THEN
         CALL PIXRSETC (MEMR(ADD), CONST, NT)
      ELSE IF (T.EQ.'D') THEN
         CALL PIXDSETC (MEMD(ADD), CONST, NT)
      ELSE IF (T.EQ.'X') THEN
         CALL PIXXSETC (MEMX(ADD), CONST, NT)
      ELSE IF (T.EQ.'I') THEN
         CALL PIXISETC (MEMI(ADD), CONST, NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1     'Array type not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
