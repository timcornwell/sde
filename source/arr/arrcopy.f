C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrcopy.f	1.6    7/31/94
C
      SUBROUTINE ARRCOPY (A1, A2)
C
CD Copy one array to another: A2 = A1
C
C	A1	REAL	input	Name of array
C	A2	REAL	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Add character arrays, and changed check for axis agreement
C       to include only real axes
C				D.S.Briggs	Oct 28 1992
C	Did not previously consider PIXLCOPY call
C				M.A.Holdaway	March 4 1993
C	Check for destination with DATEXIAR instead of DATEXIST
C				D.S.Briggs	July 31 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRCOPY')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, RN1, RN2, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM)
      INTEGER		ADD1, ADD2, NT
C
      INTEGER		CRDRNAX
      LOGICAL		DATEXIAR
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      RN1 = CRDRNAX (N1, NAXIS1)
      NT = 1
      DO 10 I = 1, RN1
         NAXIS1(I) = MAX(1, NAXIS1(I))
         NT = NT * NAXIS1(I)
  10  CONTINUE
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIAR(A2)) THEN
         T2 = T1
	 N2 = N1
         DO 20 I = 1, N2
            NAXIS2(I) = NAXIS1(I)
  20     CONTINUE
         CALL DATMAKAR (A2, N2, NAXIS2, T2, ADD2)
      ELSE
         CALL DATGETAR (A2, N2, NAXIS2, T2, ADD2)
         RN2 = CRDRNAX (N2, NAXIS2)
         IF (RN1.NE.RN2) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Different number of real axes')
            GO TO 999
         END IF
         IF (T1.NE.T2) THEN
            WRITE (MESSAGE, 1100) T1, T2
 1100       FORMAT (
     1         'Array types for image 1 and output image disagree : ',
     2         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         END IF
         DO 30 I = 1,RN2
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
         CALL PIXRCOPY (MEMR(ADD1), 0, 1, MEMR(ADD2), 0, 1, NT)
      ELSE IF (T1.EQ.'X') THEN
         CALL PIXXCOPY (MEMX(ADD1), 1, MEMX(ADD2), 1, NT)
      ELSE IF (T1.EQ.'I') THEN
         CALL PIXICOPY (MEMI(ADD1), 1, MEMI(ADD2), 1, NT)
      ELSE IF (T1.EQ.'D') THEN
         CALL PIXDCOPY (MEMD(ADD1), 1, MEMD(ADD2), 1, NT)
      ELSE IF (T1.EQ.'L') THEN
         CALL PIXLCOPY (MEML(ADD1), 1, MEML(ADD2), 1, NT)
      ELSE IF (T1.EQ.'S') THEN
         CALL PIXCCOPY (MEMC(ADD1), 1, MEMC(ADD2), 1, NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      ' Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
