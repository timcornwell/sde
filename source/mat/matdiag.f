C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matdiag.f	1.1    6/7/93
C
      SUBROUTINE MATDIAG (IN, OUT)
C
CD Create a diagonal matrix from a 1-d vector.
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
      PARAMETER		(ROUTINE = 'MATDIAG')
C
      CHARACTER*1	TIN, TOUT
      INTEGER		I, INAX, ONAX, INAXIS(SYSMXDIM),
     $			ONAXIS(SYSMXDIM), IRNAX, ORNAX, IADD, OADD
C
      LOGICAL		DATEXIST
      INTEGER		CRDRNAX
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, INAX, INAXIS, TIN, IADD)
      IRNAX = CRDRNAX(INAX,INAXIS)
C
      IF (IRNAX.GT.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input vector has too many real axes')
         GO TO 999
      END IF
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(OUT)) THEN
         TOUT = TIN
         ONAX = 2
         ONAXIS(1) = INAXIS(1)
         ONAXIS(2) = INAXIS(1)
         CALL DATMAKAR (OUT, ONAX, ONAXIS, TOUT, OADD)
      ELSE
         CALL DATGETAR (OUT, ONAX, ONAXIS, TOUT, OADD)
         ORNAX = CRDRNAX(ONAX,ONAXIS)
         IF (.NOT.((ORNAX.EQ.2).OR.
     $             ((ONAXIS(1).EQ.1).AND.(ONAXIS(2).EQ.1)))) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Output has bad number of axes')
            GO TO 999
         END IF
         IF ((ONAXIS(1).NE.INAXIS(1)).OR.(ONAXIS(2).NE.INAXIS(1))) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Output array is wrong size')
            GO TO 999
         END IF
      END IF
C
C Now do something useful
C
      CALL ARRSETCO (OUT, 0.0, 0.0)
      IF (TIN.EQ.'R') THEN
         DO 100 I = 1, ONAXIS(1)
            MEMR(OADD+(I-1)*ONAXIS(1)+I-1) = MEMR(IADD+I-1)
 100     CONTINUE
      ELSE IF (TIN.EQ.'X') THEN
         DO 110 I = 1, ONAXIS(1)
            MEMX(OADD+(I-1)*ONAXIS(1)+I-1) = MEMX(IADD+I-1)
 110     CONTINUE
      ELSE IF (TIN.EQ.'I') THEN
         DO 120 I = 1, ONAXIS(1)
            MEMI(OADD+(I-1)*ONAXIS(1)+I-1) = MEMI(IADD+I-1)
 120     CONTINUE
      ELSE IF (TIN.EQ.'D') THEN
         DO 130 I = 1, ONAXIS(1)
            MEMD(OADD+(I-1)*ONAXIS(1)+I-1) = MEMD(IADD+I-1)
 130     CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//TIN//
     1      'Not supported')
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
