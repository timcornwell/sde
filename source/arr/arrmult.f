C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrmult.f	1.5   5/4/91
C
      SUBROUTINE ARRMULT (A1, A2, A3)
C
CD Mask an array A1 by A2
C
C
C	A1	REAL	input	Name of input array
C	A2	REAL	input	Name of mask array
C	A3	REAL	input	Name of output masked array
C Audit trail:
C	Added 2D masking
C				T.J. Cornwell April 4 1991
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2, A3
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRMULT')
C
      CHARACTER*1	T1, T2, T3
      INTEGER		I, N1, N2, N3, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM), NAXIS3(SYSMXDIM)
      INTEGER		ADD1, ADD2, ADD3, NT, OFFZ, IZ
      LOGICAL		DATEXIST, TWODMASK
      INTEGER		CRDRNAX, RNAX
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      CALL DATGETAR (A2, N2, NAXIS2, T2, ADD2)
      TWODMASK=(NAXIS1(3).GT.1).AND.(NAXIS2(3).EQ.1)
      NT = 1
      IF(TWODMASK) THEN
         RNAX = 2
      ELSE
         RNAX = MAX (CRDRNAX(N1, NAXIS1), CRDRNAX(N2, NAXIS2))
      ENDIF
      DO 10 I = 1, RNAX
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
 1000    FORMAT ('Array types for images 1 and 2 disagree : ',
     1      A1,1X,A1)
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
      IF(TWODMASK) THEN
         IF (T1.EQ.'R') THEN
            DO 100 IZ = 1, NAXIS1(3)
               OFFZ = NT * (IZ-1)
               CALL PIXRMULT (MEMR(ADD1+OFFZ), MEMR(ADD2),
     $            MEMR(ADD3+OFFZ), NT)
 100        CONTINUE
         ELSE IF (T1.EQ.'X') THEN
            DO 110 IZ = 1, NAXIS1(3)
               OFFZ = NT * (IZ-1)
               CALL PIXXMULT (MEMX(ADD1+OFFZ), MEMX(ADD2),
     $            MEMX(ADD3+OFFZ), NT)
 110        CONTINUE
         ELSE IF (T1.EQ.'I') THEN
            DO 120 IZ = 1, NAXIS1(3)
               OFFZ = NT * (IZ-1)
               CALL PIXIMULT (MEMI(ADD1+OFFZ), MEMI(ADD2),
     $            MEMI(ADD3+OFFZ), NT)
 120        CONTINUE
         ELSE IF (T1.EQ.'D') THEN
            DO 130 IZ = 1, NAXIS1(3)
               OFFZ = NT * (IZ-1)
               CALL PIXDMULT (MEMD(ADD1+OFFZ), MEMD(ADD2),
     $            MEMD(ADD3+OFFZ), NT)
 130        CONTINUE
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1         'Not supported')
         END IF
      ELSE
         IF (T1.EQ.'R') THEN
            CALL PIXRMULT (MEMR(ADD1), MEMR(ADD2), MEMR(ADD3), NT)
         ELSE IF (T1.EQ.'X') THEN
            CALL PIXXMULT (MEMX(ADD1), MEMX(ADD2), MEMX(ADD3), NT)
         ELSE IF (T1.EQ.'I') THEN
            CALL PIXIMULT (MEMI(ADD1), MEMI(ADD2), MEMI(ADD3), NT)
         ELSE IF (T1.EQ.'D') THEN
            CALL PIXDMULT (MEMD(ADD1), MEMD(ADD2), MEMD(ADD3), NT)
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1         'Not supported')
         END IF
      ENDIF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
