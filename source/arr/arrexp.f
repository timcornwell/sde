C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrexp.f	1.1	 7/1/91
C
      SUBROUTINE ARREXP (IN, RFACT, IFACT, OUT)
C
CD Array level Exponentiation:  OUT(i) = EXP(FACT * IN(i))
C
C	IN	CH*(*)	input	Name of array
C	RFACT	REAL	input	constant in exponentaition
C	IFACT	REAL	input	imaginary part of constant
C				(only used if OUT is complex;
C				OR, if IFACT.NE.0 and .NOT.DATEXIST(OUT),
C				make a complex OUT)
C	OUT	CH*(*)	input	Name of array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 28 1991
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
      REAL		RFACT, IFACT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARREXP')
C
      COMPLEX		FACT
      CHARACTER*1	INT, OUTT
      INTEGER		I, INN, OUTN, INAXIS(SYSMXDIM),
     1			ONAXIS(SYSMXDIM)
      INTEGER		IADD, OADD, NT
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, INN, INAXIS, INT, IADD)
      NT = 1
      DO 10 I = 1, INN
         NT = NT * INAXIS(I)
  10  CONTINUE
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(OUT)) THEN
         IF (IFACT .NE. 0.0) THEN
            OUTT = 'X'
         ELSE
            OUTT = INT
         ENDIF
	 OUTN = INN
         DO 20 I = 1, OUTN
            ONAXIS(I) = INAXIS(I)
  20     CONTINUE
         CALL DATMAKAR (OUT, OUTN, ONAXIS, OUTT, OADD)
      ELSE
         CALL DATGETAR (OUT, OUTN, ONAXIS, OUTT, OADD)
         IF (INN.NE.OUTN) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Different number of axes')
            GO TO 999
         END IF
         DO 30 I = 1,OUTN
            IF (ONAXIS(I).NE.INAXIS(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     1            'Axes disagree')
               GO TO 999
            END IF
  30     CONTINUE
      END IF
C
C Check array types
C
      IF (IFACT .NE. 0.0) THEN
         IF (OUTT .NE. 'X') THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Outtype must be X for a complex factor')
            GOTO 990
         ENDIF
      ELSE
         IF (OUTT .NE. INT) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Intype and Outtype disagree')
            GOTO 990
         ENDIF
      ENDIF
C
C Call appropriate routine
C
      IF (INT.EQ.'R' .AND. OUTT.EQ.'R') THEN
         CALL PIXREXP (MEMR(IADD), RFACT, MEMR(OADD), NT)
      ELSE IF (INT .EQ. 'R' .AND. OUTT.EQ. 'X') THEN
         FACT = CMPLX (RFACT, IFACT)
         CALL PIXRXEXP (MEMR(IADD), FACT, MEMX(OADD), NT)
      ELSE IF (INT .EQ. 'X' .AND. OUTT.EQ. 'X') THEN
         FACT = CMPLX (RFACT, IFACT)
         CALL PIXXEXP (MEMX(IADD), FACT, MEMX(OADD), NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//INT//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
