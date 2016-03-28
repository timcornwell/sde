C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrmask.f	1.1    12/20/90
C
      SUBROUTINE ARRMASK (A1, A2, AMIN, AMAX, FLUX1, FLUX2, FLUX3)
C
CD On output, sets pixels between AMIN and AMAX to FLUX1, others to FLUX2
C
C	A1	CH*(*)	input	Name of array
C	A2	CH*(*)	input	Name of output array
C	AMIN	REAL	in	Min Value
C	AMAX 	REAL	in	Max Value
C	FLUX1	REAL	in	Assignment value: Too Low
C	FLUX2	REAL	in	Assignment value: In Range
C	FLUX3	REAL	in	Assignment value: Too High
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Sept 16 1990
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2
      REAL		AMIN, AMAX, FLUX1, FLUX2, FLUX3
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRMASK')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM)
      INTEGER		ADD1, ADD2, NT
      LOGICAL		DATEXIST
      DATA		NAXIS1	/SYSMXDIM * 1/
      DATA		NAXIS2	/SYSMXDIM * 1/
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(A2)) THEN
         T2 = T1
	 N2 = N1
         DO 20 I = 1, N2
            NAXIS2(I) = NAXIS1(I)
  20     CONTINUE
         CALL DATMAKAR (A2, N2, NAXIS2, T2, ADD2)
      ELSE
         CALL DATGETAR (A2, N2, NAXIS2, T2, ADD2)
         DO 25 I = 1, MAX(N1, N2)
            IF (NAXIS1(I).NE.NAXIS2(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes do not match')
               GO TO 999
            END IF
 25         CONTINUE
         IF (T1.NE.T2) THEN
            WRITE (MESSAGE, 1100) T1, T2
 1100       FORMAT (
     1         'Array types for image 1 and output image disagree : ',
     2         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         END IF
         DO 30 I = 1,N2
            IF (NAXIS2(I).NE.NAXIS1(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     1            'Axes disagree')
               GO TO 999
            END IF
  30     CONTINUE
      END IF
      NT = 1
      DO 40 I = 1, N1
         NT = NT * NAXIS1(I)
 40   CONTINUE
C
C Call appropriate routine
C
      IF (AMIN .GT. AMAX) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Min is greater than Max')
         GOTO 990
      ENDIF
      IF (T1.EQ.'R') THEN
         CALL PIXRMASK (MEMR(ADD1), MEMR(ADD2), NT, AMIN, AMAX, FLUX1, 
     $      FLUX2, FLUX3)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
