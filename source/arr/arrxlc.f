C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrxlc.f	1.1	 11/20/92
C
      SUBROUTINE ARRXLC (A1, W1, A2, W2, A3)
C
CD Linear combination of two arrays with complex A's
C
C
C	A1	CH*(*)	input	Name of array
C	W1	X	input	Weight
C	A2	CH*(*)	input	Name of array
C	W2	X	input	Weight
C	A3	CH*(*)	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Oct 15 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2, A3
      COMPLEX		W1, W2
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRXLC')
C
      CHARACTER*1	T1, T2, T3
      INTEGER		I, N1, N2, N3, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM), NAXIS3(SYSMXDIM)
      INTEGER		ADD1, ADD2, ADD3, NT
      LOGICAL		DATEXIST
      DATA		NAXIS1	/SYSMXDIM * 1/
      DATA		NAXIS2	/SYSMXDIM * 1/
      DATA		NAXIS3	/SYSMXDIM * 1/
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      CALL DATGETAR (A2, N2, NAXIS2, T2, ADD2)
      NT = 1
      DO 10 I = 1, MAX(N1, N2)
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
 1000    FORMAT ('Array types for images 1 and 2 disagree : ',A1,1X,A1)
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
         DO 25 I = 1, MAX(N1, N2)
            IF (NAXIS1(I).NE.NAXIS3(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes do not match')
               GO TO 999
            END IF
 25         CONTINUE
         IF (T1.NE.T3) THEN
            WRITE (MESSAGE, 1100) T1, T3
 1100       FORMAT (
     1         'Array types for image 1 and output image disagree : ',
     2         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
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
      IF (T1.EQ.'X') THEN
         CALL PIXXXLC (MEMX(ADD1), W1, MEMX(ADD2), W2, MEMX(ADD3), 
     1      NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
