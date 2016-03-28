C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsubtr.f	1.3    11/7/90
C
      SUBROUTINE ARRSUBTR (A1, A2, A3)
C
CD Subtract two REAL arrays : A3 = A1 - A2
C
C
C	A1	REAL	input	Name of array
C	A2	REAL	input	Name of array
C	A3	REAL	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2, A3
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSUBTR')
C
      CHARACTER*1	T1, T2, T3
      INTEGER		I, N1, N2, N3, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM), NAXIS3(SYSMXDIM)
      INTEGER		ADD1, ADD2, ADD3, NT
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      CALL DATGETAR (A2, N2, NAXIS2, T2, ADD2)
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
C Check types of arrays
C
      IF (T1.NE.T2) THEN
         WRITE (MESSAGE, 1000) T1, T2
 1000    FORMAT ('Array types for arrays 1 and 2 disagree : ',A1,1X,A1)
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(A3)) THEN
         T3 = T1
	 N3 = N1
         DO 20 I = 1, N3
            NAXIS3(I) = NAXIS1(I)
  20     CONTINUE
         CALL DATMAKAR (A3, N3, NAXIS3, T3, ADD3)
      ELSE
         CALL DATGETAR (A3, N3, NAXIS3, T3, ADD3)
         IF (N1.NE.N3) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Different number of axes')
            GO TO 999
         END IF
         IF (T1.NE.T3) THEN
            WRITE (MESSAGE, 1100) T1, T3
 1100       FORMAT (
     1         'Array types for array 1 and output array disagree : ',
     2         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
            GO TO 999
         END IF
         DO 30 I = 1,N3
            IF (NAXIS3(I).NE.NAXIS1(I)) THEN
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
         CALL PIXRSUBT (MEMR(ADD1), MEMR(ADD2), MEMR(ADD3), NT)
      ELSE IF (T1.EQ.'X') THEN
         CALL PIXXSUBT (MEMX(ADD1), MEMX(ADD2), MEMX(ADD3), NT)
      ELSE IF (T1.EQ.'I') THEN
         CALL PIXISUBT (MEMI(ADD1), MEMI(ADD2), MEMI(ADD3), NT)
      ELSE IF (T1.EQ.'D') THEN
         CALL PIXDSUBT (MEMD(ADD1), MEMD(ADD2), MEMD(ADD3), NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
