C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arr3corr.f	1.4    11/7/90
C
      SUBROUTINE ARR3CORR (A1, A2, A3)
C
CD Triple Correlate two arrays together
C
C	a1	CH*(*)	input	Name of array
C	a2	CH*(*)	input	Name of array
C	a3	CH*(*)	input	Name of array
C Audit trail:
C	Cloned from arrcorr
C				R.G. Marson     Aug 31 1990
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2, A3
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARR3CORR')
C
      CHARACTER*1	T1, T2, T3
      INTEGER		I, N1, N2, N3, NAXIS1(SYSMXDIM)
      INTEGER           NAXIST(SYSMXDIM)
      INTEGER           NAXIS2(SYSMXDIM), NAXIS3(SYSMXDIM)
      INTEGER		ADD1, ADD2, ADD3, ADDT, NT1, NT2, NT
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
      NT1 = 1
      NT2 = 1
      DO I = 1, N1
         IF (NAXIS1(I).EQ.1) N1 = N1 - 1
         IF (NAXIS2(I).EQ.1) N2 = N2 - 1
         NT1 = NT1 * NAXIS1(I)
         NT2 = NT2 * NAXIS2(I)
      END DO
      IF ((N1.NE.1).OR.(N2.NE.1).OR.
     $     (NAXIS1(1).EQ.1).OR.(NAXIS2(1).EQ.1)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $        'Can only triple correlate 1 D arrays')
         GOTO 999
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
      IF (T1.NE.'R') THEN 
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $        'Can only Correlate Real Arrays')
         GO TO 999
      END IF
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(A3)) THEN
         T3 = T1
	 n3 = 2
         DO 20 I = 1, 2
            NAXIS3(I) = NT1 + NT2 - 1
  20     CONTINUE
         CALL DATMAKAR (A3, N3, NAXIS3, T3, ADD3)
      ELSE
         CALL DATGETAR (A3, N3, NAXIS3, T3, ADD3)
         DO I = 1, N3
            IF (NAXIS3(I).EQ.1) N3 = N3 - 1
         END DO
         IF (N3.NE.2) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Different number of axes')
            GO TO 999
         END IF
         IF (T1.NE.T3) THEN
            WRITE (MESSAGE, 1100) T1, T3
 1100       FORMAT (
     1         'Array types for image 1 and output image disagree : ',
     2         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         END IF
         DO 30 I = 1,2
            IF (NAXIS3(I).NE.(NT1 + NT2 - 1)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     1            'Axes disagree')
               GO TO 999
            END IF
  30     CONTINUE
      END IF
C
C Make a tempory or working array
C
      NAXIST(1) = NT1
      CALL DATMAKAR('arr3corr', 1, NAXIST, 'R', ADDT)
C
C Zero the output array
C
      NT = NT1 + NT2 - 1
      CALL PIXRSETC(MEMR(ADD3), 0.0, NT**2)
C
C Do this loop for all displacements
C
      DO I = 1, NT
C
C Multiply the arrays together
C
         CALL PIXRSETC(MEMR(ADDT), 0.0, NT1)

         CALL PIXRMULT(MEMR(ADD1 + MAX(0, I - NT2)), 
     $                MEMR(ADD2 + MAX(0, NT2 - I)), 
     $                MEMR(ADDT + MAX(0, I - NT2)),
     $                MIN(NT1, NT2, I, NT - I + 1))
C
C Correlate the arrays
C
         CALL PIXRCORR (MEMR(ADD1), NT1, MEMR(ADDT), NT1, 
     $                  MEMR(ADD3+(I-1)*NT))
      END DO
C
C Delete the tempory array
C
      CALL DATDELAR('arr3corr')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
