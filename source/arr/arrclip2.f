C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrclip2.f	1.1	 8/16/91
C
      SUBROUTINE ARRCLIP2 (A1, A2, AMIN1, AMIN2, AMAX1, AMAX2, A3)
C
CD Clip to value:   If A1(i) < AMIN1, set A3(i) to AMIN2 (else A3(i)=A2(I))
C                   If A1(i) > AMAX1, set A3(i) to AMAX2 (else A3(i)=A2(I))

C
C	A1	CH*(*)	input	Name of array
C	A2	CH*(*)	input	Name of array
C	AMIN1	REAL	input	Min allowed
C	AMIN2	REAL	input	Min allowed
C	AMAX1	REAL	input	Max allowed
C	AMAX2	REAL	input	Max allowed
C	A3	CH*(*)	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	July 11 1991
C				Happy Partial Eclipse Day
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2, A3
      REAL		AMIN1, AMAX1, AMIN2, AMAX2
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRCLIP2')
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
C
C Check the two input arrays
C
      IF (N1.NE.N2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Different number of axes')
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
 10   CONTINUE
      IF (T1.NE.T2) THEN
         WRITE (MESSAGE, 1000) T1, T2
 1000    FORMAT ('Array types for images 1 and 2 disagree : ',
     1      A1,1X,A1)
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Check the OUT against IN1
C
      IF (DATEXIST (A3)) THEN
         CALL DATGETAR (A3, N3, NAXIS3, T3, ADD3)
         IF (N1.NE.N3) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Different number of axes')
               GO TO 999
         END IF
         NT = 1
         DO 20 I = 1, N1
            IF (NAXIS1(I).NE.NAXIS3(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes do not match')
               GO TO 999
            ELSE
               NT = NT * NAXIS1(I)
            END IF
  20     CONTINUE
         IF (T1.NE.T3) THEN
            WRITE (MESSAGE, 1010) T1, T3
 1010       FORMAT ('Array types for images 1 and 3 disagree : ',
     1         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
            GO TO 999
         END IF
      ELSE
C
C OR, Make output array if it does not exist
C
         T3 = T1
	 N3 = N1
         DO 50 I = 1, N1
            NAXIS3(I) = NAXIS1(I)
  50     CONTINUE
         CALL DATMAKAR (A3, N3, NAXIS3, T3, ADD3)
      END IF
C
C Call appropriate routine
C
      IF (T1.EQ.'R') THEN
         CALL PIXRCLP2 (MEMR(ADD1), MEMR(ADD2), AMIN1, AMIN2, 
     $      AMAX1, AMAX2, MEMR(ADD3), NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
