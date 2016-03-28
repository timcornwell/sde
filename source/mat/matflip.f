C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matflip.f	1.1    6/16/94
C
      SUBROUTINE MATFLIP (IN, OUT)
C
CD Flip a 2-D matrix around 1st axis
C
C	IN	CH*(*)	input	Name of input array
C	OUT	CH*(*)	input	Name of output array
C
C Audit trail:
C	Initial version
C				D.S. Briggs	5 Nov 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MATFLIP')
C
      CHARACTER*1	TIN, TOUT
      INTEGER		I, J, INAX, ONAX, INAXIS(SYSMXDIM),
     $			ONAXIS(SYSMXDIM), IRNAX, ORNAX, IADD, OADD
      LOGICAL		INPLACE
      INTEGER		ITMP
      REAL		RTMP
      COMPLEX		XTMP
      DOUBLE PRECISION	DTMP
C
      LOGICAL		DATEXIST
      INTEGER		CRDRNAX
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, INAX, INAXIS, TIN, IADD)
      IRNAX = CRDRNAX(INAX,INAXIS)
C
      IF (IRNAX.NE.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array 1 has bad number of axes')
         GO TO 999
      END IF
      IF (INAX.EQ.1) INAXIS(2) = 1
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(OUT)) THEN
         TOUT = TIN
         ONAXIS(1) = INAXIS(1)
         ONAXIS(2) = INAXIS(2)
         IF (ONAXIS(2).EQ.1) THEN
            ONAX = 1
         ELSE
            ONAX = 2
         END IF
         CALL DATMAKAR (OUT, ONAX, ONAXIS, TOUT, OADD)
      ELSE
         CALL DATGETAR (OUT, ONAX, ONAXIS, TOUT, OADD)
         ORNAX = CRDRNAX(ONAX,ONAXIS)
         IF (ORNAX.NE.2) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Output has bad number of axes')
            GO TO 999
         END IF
         IF (TIN.NE.TOUT) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Array types for input and output image disagree')
            GO TO 999
         END IF
      END IF
      INPLACE = .FALSE.
      IF (OADD.EQ.IADD) THEN
         IF (ONAXIS(1).NE.ONAXIS(2)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Cannot transpose non-square arrays in place','I')
            GO TO 999
         END IF
         INPLACE = .TRUE.
      END IF
C
C Now do something useful
C
      IF (.NOT.INPLACE) CALL ARRCOPY (IN, OUT)
C
      IF (TIN.EQ.'R') THEN
         DO 120 J = 1, ONAXIS(2)
            DO 110 I = 1, ONAXIS(1)/2
               RTMP = MEMR(OADD+(J-1)*ONAXIS(1)+I-1)
               MEMR(OADD+(J-1)*ONAXIS(1)+I-1) =
     $            MEMR(OADD+(J-1)*ONAXIS(1)+(ONAXIS(1)-I+1)-1)
               MEMR(OADD+(J-1)*ONAXIS(1)+(ONAXIS(1)-I+1)-1) = RTMP
 110        CONTINUE
 120     CONTINUE
      ELSE IF (TIN.EQ.'X') THEN
         DO 140 J = 1, ONAXIS(2)
            DO 130 I = 1, ONAXIS(1)/2
               XTMP = MEMX(OADD+(J-1)*ONAXIS(1)+I-1)
               MEMX(OADD+(J-1)*ONAXIS(1)+I-1) =
     $            MEMX(OADD+(J-1)*ONAXIS(1)+(ONAXIS(1)-I+1)-1)
               MEMX(OADD+(J-1)*ONAXIS(1)+(ONAXIS(1)-I+1)-1) = XTMP
 130        CONTINUE
 140     CONTINUE
      ELSE IF (TIN.EQ.'I') THEN
         DO 160 J = 1, ONAXIS(2)
            DO 150 I = 1, ONAXIS(1)/2
               ITMP = MEMI(OADD+(J-1)*ONAXIS(1)+I-1)
               MEMI(OADD+(J-1)*ONAXIS(1)+I-1) =
     $            MEMI(OADD+(J-1)*ONAXIS(1)+(ONAXIS(1)-I+1)-1)
               MEMI(OADD+(J-1)*ONAXIS(1)+(ONAXIS(1)-I+1)-1) = ITMP
 150        CONTINUE
 160     CONTINUE
      ELSE IF (TIN.EQ.'D') THEN
         DO 180 J = 1, ONAXIS(2)
            DO 170 I = 1, ONAXIS(1)/2
               DTMP = MEMD(OADD+(J-1)*ONAXIS(1)+I-1)
               MEMD(OADD+(J-1)*ONAXIS(1)+I-1) =
     $            MEMD(OADD+(J-1)*ONAXIS(1)+(ONAXIS(1)-I+1)-1)
               MEMD(OADD+(J-1)*ONAXIS(1)+(ONAXIS(1)-I+1)-1) = DTMP
 170        CONTINUE
 180     CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//TIN//
     1      'Not supported')
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
