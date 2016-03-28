C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftiparso.f	1.6 5/12/94
C
      SUBROUTINE FTIPARSO (CCARD, KEYWORD, OBJECT, RVALUE,
     1   IVALUE, DVALUE, XVALUE, LVALUE, CVALUE, TYPE)
C
CD Translate KEYWORDs to FITS cards.
C
C
C	CCARD	CH*(*)	output	FITS card in CHARACTER format
C	KEYWORD	CH*(*)	input	FITS keyword
C	OBJECT	CH*(*)	output	FITS object
C      RVALUE	REAL	input	Value
C      IVALUE	INTEGER	input	Value
C      DVALUE	DOUBLE	input	Value
C      XVALUE	COMPLEX	input	Value
C      LVALUE	LOGICAL	input	Value
C	CVALUE	CH*(*)	input	Value
C	TYPE	CH*(*)	input 	Value
C Audit trail:
C	Changed to FTIPARSO from FTSPARSE
C				T.J.Cornwell	Jan 13 1989
C	Changed CVALUE to be written as all eight characters
C				T.J. Cornwell	May 6 1994
C       Changed CVALUE to be written as minimum of eight characters
C                               S.Bhatnagar     May 9 1994
C
C--------------------------------------------------------------------
#include	"stdinc.h"
#include	"ftsinc.h"
C
      CHARACTER*(*)	CCARD, KEYWORD, OBJECT
      REAL		RVALUE
      INTEGER		IVALUE
      DOUBLE PRECISION	DVALUE
      COMPLEX		XVALUE
      LOGICAL		LVALUE
      CHARACTER*(*)	CVALUE
      CHARACTER*(*)	TYPE
      INTEGER           STRLEN
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTIPARSO')
C====================================================================
      IF (ERROR) GO TO 999
C
      IF (TYPE.EQ.'R') THEN
         WRITE (CCARD, 1000, ERR = 100) KEYWORD, RVALUE
 1000    FORMAT (A8,'=  ',3X, 1PE16.9,' /')
      ELSEIF (TYPE.EQ.'I') THEN
         WRITE (CCARD, 1100, ERR = 100) KEYWORD, IVALUE
 1100    FORMAT (A8,'=  ',11X, I8,' /')
      ELSEIF (TYPE.EQ.'D') THEN
         WRITE (CCARD, 1200, ERR = 100) KEYWORD, DVALUE
 1200    FORMAT (A8,'=  ',1X, 1PE18.11,' /')
      ELSEIF (TYPE.EQ.'L') THEN
         IF (LVALUE) THEN
            WRITE (CCARD, 1300, ERR = 100) KEYWORD
 1300       FORMAT (A8,'=  ',18X, 'T /')
         ELSE
            WRITE (CCARD, 1310, ERR = 100) KEYWORD
 1310       FORMAT (A8,'=  ', 18X, 'F /')
         END IF
      ELSEIF (TYPE.EQ.'C') THEN
         IF (CVALUE.EQ.' ') THEN
         WRITE (CCARD, 1450, ERR = 100) KEYWORD
 1450       FORMAT (A8)
         ELSE
         WRITE (CCARD, 1451, ERR = 100) KEYWORD, 
     $         CVALUE(1:MAX(8,STRLEN(CVALUE)))
 1451       FORMAT (A8,'=  ','''',A,'''')
         ENDIF
      ELSEIF (TYPE.EQ.' ') THEN
         WRITE (CCARD, 1500, ERR = 100) KEYWORD
 1500    FORMAT (A8)
      ENDIF
C
      GO TO 200
C
 100  CONTINUE
      MESSAGE = 'Cannot write keyword: '//KEYWORD//' to FITS card'
      CALL ERRREPOR (ERRLOGIC, ROUTINE, MESSAGE)
      GO TO 999
C
 200  CONTINUE
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
