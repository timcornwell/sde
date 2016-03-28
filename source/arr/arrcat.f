C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrcat.f	1.9	 7/18/97
C
      SUBROUTINE ARRCAT (A1, A2, A3)
C
CD Concatenate two one-dimensional arrays
C
C	A1	REAL	input	Name of array
C	A2	REAL	input	Name of array
C	A3	REAL	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Now can do concatenation in place
C				T.J.Cornwell	Nov 27 1990
C	Fixed String concatenations
C				T.J.Cornwell	Jan 6 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2, A3
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRCAT')
C
      CHARACTER*(SYSMXNAM)	A3T
      CHARACTER*1	T1, T2, T3
      INTEGER		I, N1, N2, N3, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM), NAXIS3(SYSMXDIM)
      INTEGER		ADD1, ADD2, ADD3
      LOGICAL		DATEXIST, INPLACE
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      CALL DATGETAR (A2, N2, NAXIS2, T2, ADD2)
      IF (N1.NE.N2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Different number of axes')
         GO TO 999
      END IF
      IF ((N1.GT.1).OR.(N2.GT.1)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Only 1-D arrays allowed')
         GO TO 999
      END IF
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
         INPLACE = .FALSE.
         T3 = T1
	 N3 = N1
         DO 20 I = 1, N3
            NAXIS3(I) = NAXIS1(I) + NAXIS2(I)
  20     CONTINUE
         CALL DATMAKAR (A3, N3, NAXIS3, T3, ADD3)
      ELSE
         IF(A1.EQ.A3) THEN
            INPLACE = .TRUE.
            T3 = T1
            N3 = N1
            DO 21 I = 1, N3
               NAXIS3(I) = NAXIS1(I) + NAXIS2(I)
 21         CONTINUE
            A3T = A3
            CALL STRAPPEN (A3T, 'TEMP')
            CALL DATMAKAR (A3T, N3, NAXIS3, T3, ADD3)
         ELSE
            MESSAGE = 'Output array exists '
            CALL STRAPPEN (MESSAGE, A3)
            CALL ERRREPOR (ERRLOGIC, ROUTINE, MESSAGE)
            GO TO 999
         END IF
      END IF
C
C Call appropriate routine
C
      IF (T1.EQ.'R') THEN
         CALL PIXRCOPY (MEMR(ADD1), 0, 1, MEMR(ADD3), 0, 1, NAXIS1(1))
         CALL PIXRCOPY (MEMR(ADD2), 0, 1, MEMR(ADD3), NAXIS1(1), 1,
     $      NAXIS2(1))
      ELSE IF (T1.EQ.'X') THEN
         CALL PIXXCOPY (MEMX(ADD1), 1, MEMX(ADD3), 1, NAXIS1(1))
         CALL PIXXCOPY (MEMX(ADD2), 1, MEMX(ADD3+NAXIS1(1)), 1,
     $      NAXIS2(1))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type not supported')
      END IF
C
      IF(INPLACE) THEN
         CALL DATDELET (A3)
         CALL DATRENAM (A3T, A3)
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
