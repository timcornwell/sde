C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matmult.f	1.1    6/7/93
C
      SUBROUTINE MATMULT (IN1, IN2, OUT)
C
CD Multiply two 2-d matricies or a 2-D matrix & 1-D vector
C
C	IN1	CH*(*)	input	Name of first input array
C	IN2	CH*(*)	input	Name of second input array
C	OUT	CH*(*)	input	Name of output masked array
C
C Audit trail:
C	Initial version
C				D.S. Briggs	5 Nov 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IN1, IN2, OUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MATMULT')
C
      CHARACTER*1	T1, T2, TO
      INTEGER		I, J, K, NAX1, NAX2, NAXO, NAXIS1(SYSMXDIM),
     $			NAXIS2(SYSMXDIM), NAXISO(SYSMXDIM),
     $    		RNAX1, RNAX2, RNAXO, ADD1, ADD2, ADDO
C
      LOGICAL		DATEXIST
      INTEGER		CRDRNAX
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN1, NAX1, NAXIS1, T1, ADD1)
      CALL DATGETAR (IN2, NAX2, NAXIS2, T2, ADD2)
      RNAX1 = CRDRNAX(NAX1,NAXIS1)
      RNAX2 = CRDRNAX(NAX2,NAXIS2)
C
      IF ((RNAX1.GT.2).OR.(NAX1.LT.2)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array 1 has bad number of axes')
         GO TO 999
      END IF
      IF (RNAX2.GT.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array 2 has too many axes')
         GO TO 999
      END IF
      IF (NAXIS1(2).NE.NAXIS2(1)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array axes are incompatable')
         GO TO 999
      END IF
      IF (T1.NE.T2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array types disagree')
         GO TO 999
      END IF
      IF (NAX1.EQ.1) NAXIS1(2) = 1
      IF (NAX2.EQ.1) NAXIS2(2) = 1
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(OUT)) THEN
         TO = T1
         NAXISO(1) = NAXIS1(1)
         IF (NAX2.EQ.1) THEN
            NAXO = 1
            NAXISO(2) = 1
         ELSE
            NAXO = 2
            NAXISO(2) = NAXIS2(2)
         END IF
         CALL DATMAKAR (OUT, NAXO, NAXISO, TO, ADDO)
      ELSE
         CALL DATGETAR (OUT, NAXO, NAXISO, TO, ADDO)
         RNAXO = CRDRNAX(NAXO,NAXISO)
         IF ((RNAXO.GT.2).OR.((RNAXO.EQ.1).AND.(NAXO.LT.2))) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Output has bad number of axes')
            GO TO 999
         END IF
         IF (T1.NE.TO) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Array types for image 1 and output image disagree')
            GO TO 999
         END IF
      END IF
C
C Now do something useful
C
      IF (T1.EQ.'R') THEN
         DO 120 I = 1, NAXISO(1)
            DO 110 J = 1, NAXISO(2) 
               MEMR(ADDO+(J-1)*NAXISO(1)+I-1) = 0.0
               DO 100 K = 1, NAXIS1(2)
                  MEMR(ADDO+(J-1)*NAXISO(1)+I-1) =
     $               MEMR(ADDO+(J-1)*NAXISO(1)+I-1) +
     $               MEMR(ADD1+(K-1)*NAXIS1(1)+I-1) *
     $               MEMR(ADD2+(J-1)*NAXIS2(1)+K-1)
 100           CONTINUE
 110        CONTINUE
 120     CONTINUE
      ELSE IF (T1.EQ.'X') THEN
         DO 150 I = 1, NAXISO(1)
            DO 140 J = 1, NAXISO(2) 
               MEMX(ADDO+(J-1)*NAXISO(1)+I-1) = 0.0
               DO 130 K = 1, NAXIS1(2)
                  MEMX(ADDO+(J-1)*NAXISO(1)+I-1) =
     $               MEMX(ADDO+(J-1)*NAXISO(1)+I-1) +
     $               MEMX(ADD1+(K-1)*NAXIS1(1)+I-1) *
     $               MEMX(ADD2+(J-1)*NAXIS2(1)+K-1)
 130           CONTINUE
 140        CONTINUE
 150     CONTINUE
      ELSE IF (T1.EQ.'I') THEN
         DO 180 I = 1, NAXISO(1)
            DO 170 J = 1, NAXISO(2) 
               MEMI(ADDO+(J-1)*NAXISO(1)+I-1) = 0
               DO 160 K = 1, NAXIS1(2)
                  MEMI(ADDO+(J-1)*NAXISO(1)+I-1) =
     $               MEMI(ADDO+(J-1)*NAXISO(1)+I-1) +
     $               MEMI(ADD1+(K-1)*NAXIS1(1)+I-1) *
     $               MEMI(ADD2+(J-1)*NAXIS2(1)+K-1)
 160           CONTINUE
 170        CONTINUE
 180     CONTINUE
      ELSE IF (T1.EQ.'D') THEN
         DO 210 I = 1, NAXISO(1)
            DO 200 J = 1, NAXISO(2) 
               MEMD(ADDO+(J-1)*NAXISO(1)+I-1) = 0.0
               DO 190 K = 1, NAXIS1(2)
                  MEMD(ADDO+(J-1)*NAXISO(1)+I-1) =
     $               MEMD(ADDO+(J-1)*NAXISO(1)+I-1) +
     $               MEMD(ADD1+(K-1)*NAXIS1(1)+I-1) *
     $               MEMD(ADD2+(J-1)*NAXIS2(1)+K-1)
 190           CONTINUE
 200        CONTINUE
 210     CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
