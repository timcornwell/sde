C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrcppl.f	1.1    12/11/92
C
      SUBROUTINE ARRCPPL (A1, A2, P, PVAL)
C
CD Copy one array hyperplane to another: A2 = A1
C
C	A1	REAL	input	Name of array
C	A2	REAL	input	Name of array
C	P	INTEGER	input	Dimension of A2 to be held constant
C	PVAL	INTEGER	input	Value of pixel in dimension P
C
C This is rather slow for just an array copy, but it's general.  If it
C turns out to be the limiting factor in a task, we're in trouble!
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Oct 17 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2
      INTEGER		P, PVAL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRCPPL')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, R1, R2, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM)
      INTEGER		ADD1, ADD2
      INTEGER		IADD, I1, I2, I3, I4, I5, I6, I7
      INTEGER		JADD, J1, J2, J3, J4, J5, J6, J7
C
      INTEGER		CRDRNAX
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      CALL DATGETAR (A2, N2, NAXIS2, T2, ADD2)
      R1 = CRDRNAX (N1, NAXIS1)
      R2 = CRDRNAX (N2, NAXIS2)
      IF (ERROR) GO TO 990
C
      IF ((R1+1).NE.R2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Incorrect number of real axes')
         GO TO 999
      END IF
      IF (T1.NE.T2) THEN
         MESSAGE = 'Array types for input and output image disagree : '
     $      // T1 // ' ' // T2
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
      END IF
      IF ((P.LT.1).OR.(P.GT.R2)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Bad axis selected')
         GO TO 999
      END IF
      IF ((PVAL.LT.1).OR.(PVAL.GT.NAXIS2(P))) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'PVAL not in range of axis')
         GO TO 999
      END IF
      DO 5 I = 1,R1
         IF (I.GE.P) THEN
            IF (NAXIS2(I+1).NE.NAXIS1(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes disagree')
               GO TO 999
            END IF
         ELSE
            IF (NAXIS2(I).NE.NAXIS1(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes disagree')
               GO TO 999
            END IF
         END IF
 5    CONTINUE
C
      DO 10 I = N1+1, SYSMXDIM
         NAXIS1(I) = 1
 10   CONTINUE
      DO 20 I = N2+1, SYSMXDIM
         NAXIS2(I) = 1
 20   CONTINUE
C
C I know, this is *grotesque* code.  Such are the realities of life.
C At least it's flexible.
C
      DO 110 I1 = 0, NAXIS1(1)-1
         IF (P.GT.1) THEN
            J1 = I1
         ELSE
            J1 = PVAL - 1
         END IF
         DO 120 I2 = 0, NAXIS1(2)-1
            IF (P.GT.2) THEN
               J2 = I2
            ELSE IF (P.LT.2) THEN
               J2 = I1
            ELSE
               J2 = PVAL - 1
            END IF
            DO 130 I3 = 0, NAXIS1(3)-1
               IF (P.GT.3) THEN
                  J3 = I3
               ELSE IF (P.LT.3) THEN
                  J3 = I2
               ELSE
                  J3 = PVAL - 1
               END IF
               DO 140 I4 = 0, NAXIS1(4)-1
                  IF (P.GT.4) THEN
                     J4 = I4
                  ELSE IF (P.LT.4) THEN
                     J4 = I3
                  ELSE
                     J4 = PVAL - 1
                  END IF
                  DO 150 I5 = 0, NAXIS1(5)-1
                     IF (P.GT.5) THEN
                        J5 = I5
                     ELSE IF (P.LT.5) THEN
                        J5 = I4
                     ELSE
                        J5 = PVAL - 1
                     END IF
                     DO 160 I6 = 0, NAXIS1(6)-1
                        IF (P.GT.6) THEN
                           J6 = I6
                        ELSE IF (P.LT.6) THEN
                           J6 = I5
                        ELSE
                           J6 = PVAL - 1
                        END IF
                        DO 170 I7 = 0, NAXIS1(7)-1
                           IF (P.LT.7) THEN
                              J7 = I6
                           ELSE
                              J7 = PVAL - 1
                           END IF
                           IADD = I1+(I2+(I3+(I4+(I5+(I6+I7*NAXIS1(6))
     $                        *NAXIS1(5))*NAXIS1(4))*NAXIS1(3))
     $                        *NAXIS1(2))*NAXIS1(1)
                           JADD = J1+(J2+(J3+(J4+(J5+(J6+J7*NAXIS2(6))
     $                        *NAXIS2(5))*NAXIS2(4))*NAXIS2(3))
     $                        *NAXIS2(2))*NAXIS2(1)
                           IF (T1.EQ.'R') THEN
                              MEMR(ADD2+JADD) = MEMR(ADD1+IADD)
                           ELSE IF (T1.EQ.'D') THEN
                              MEMD(ADD2+JADD) = MEMD(ADD1+IADD)
                           ELSE IF (T1.EQ.'I') THEN
                              MEMI(ADD2+JADD) = MEMI(ADD1+IADD)
                           ELSE IF (T1.EQ.'X') THEN
                              MEMX(ADD2+JADD) = MEMX(ADD1+IADD)
                           ELSE
                              CALL ERRREPOR (ERRBDARG, ROUTINE,
     $                           'Array type not supported')
                              GO TO 999
                           END IF
 170                    CONTINUE
 160                 CONTINUE
 150              CONTINUE
 140           CONTINUE
 130        CONTINUE
 120     CONTINUE
 110  CONTINUE
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
