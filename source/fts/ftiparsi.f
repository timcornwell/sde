C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftiparsi.f	1.4    11/9/90
C
      SUBROUTINE FTIPARSI (CCARD, KEYWORD, OBJECT, RVALUE,
     1   IVALUE, DVALUE, XVALUE, LVALUE, CVALUE, TYPE)
C
CD Split FITS card into into KEYWORD and OBJECT strings : i.e. 
C KEYWORD = OBJECT. Try to read the object card into a value field.
C
C
C	CCARD	CH*(*)	output	FITS card in CHARACTER format
C	KEYWORD	CH*(*)	output	FITS keyword
C	OBJECT	CH*(*)	output	FITS object
C      RVALUE	REAL	output	Value
C      IVALUE	INTEGER	output	Value
C      DVALUE	DOUBLE	output	Value
C      XVALUE	COMPLEX	output	Value
C      LVALUE	LOGICAL	output	Value
C	CVALUE	CH*(*)	output	Value
C	TYPE	CH*(*)	output 	Value
C Audit trail:
C	Changed to FTIPARSI from FTIPARSEIN
C				T.J.Cornwell	Jan 13 1989
C       Modified to read TYPE 'C' and ignore the comments
C                               R.G. Marson     Nov 9 1990
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
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
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTIPARSI')
C
      CHARACTER		LOBJECT*(FTSLNOBJ)
      INTEGER		I, J, POSE, POSSL, POSDOT, STRLEN, LREAL,
     1   POSQ1, POSQ2, IKEY
      DATA		LREAL	/ 11 /
C====================================================================
      KEYWORD = ' '
      OBJECT = ' '
      TYPE = ' '
      RVALUE = 0.0
      IVALUE = 0
      DVALUE = 0.0D0
      XVALUE = 0.0
      LVALUE = .FALSE.
      CVALUE = ' '
C
      IF (ERROR) GO TO 999
C
C Find Keyword first
C
      DO 20 I = 1,LEN(CCARD)
         IF (CCARD(I:I).EQ.'/') THEN
            GO TO 25
         END IF
         IF (CCARD(I:I).NE.' ') THEN
            IF((CCARD(I:I).GE.'A').AND.(CCARD(I:I).LE.'Z')) THEN
               GO TO 30
            ELSE
               GO TO 25
            END IF
         END IF
  20  CONTINUE
C
C We could not find the Keyword: give it up as a bad job
C
  25  CONTINUE
      GO TO 999
C
C We found the Keyword
C
  30  CONTINUE
      KEYWORD = CCARD (I:I+7)
      OBJECT = CCARD(I+8:)
C
C Do not bother parsing some cards
C
      IF ((KEYWORD.EQ.'HISTORY').OR.(KEYWORD.EQ.'COMMENT')) THEN
         GO TO 999
      END IF
C
C Now find the Object
C
      DO 40 J = I+9, LEN(CCARD)
          IF (CCARD(J:J).NE.' ') THEN
            GO TO 45
          END IF
  40  CONTINUE
      OBJECT = ' '
      GO TO 999
C
C Stripped blanks OK
C
  45  CONTINUE
      LOBJECT = CCARD (J:LEN(CCARD))
C
C Is this a keyword which we know about?
C
      TYPE = ' '
      DO 37 IKEY = 1,FTSNKEYS
        IF (KEYWORD.EQ.FTSKEYS(1,IKEY)) TYPE = FTSKEYS(2,IKEY)
  37  CONTINUE
C
      IF (TYPE.EQ.'R') THEN
         CALL STRATOF (LOBJECT, STRLEN (LOBJECT), RVALUE)
         GO TO 999
      ELSEIF (TYPE.EQ.'I') THEN
         CALL STRATOI (LOBJECT, STRLEN (LOBJECT), IVALUE)
         GO TO 999
      ELSEIF (TYPE.EQ.'D') THEN
         CALL STRATOD (LOBJECT, STRLEN (LOBJECT), DVALUE)
         GO TO 999
      ELSEIF (TYPE.EQ.'L') THEN
         READ (LOBJECT, 1000, ERR = 100) LVALUE
         GO TO 999
      ELSEIF (TYPE.EQ.'C') THEN
C
C Strip off the quotes around the string
C
         IF(STRLEN(LOBJECT).LE.2) THEN
            CVALUE=' '
         ELSE
            POSQ1 = INDEX (LOBJECT, '''')
            POSQ2 = POSQ1 + INDEX (LOBJECT(POSQ1+1:), '''')
            IF ((POSQ1.GT.0).AND.(POSQ2.GT.POSQ1)) THEN
               IF (POSQ2.EQ.POSQ1+1) THEN
                  CVALUE = ' '
               ELSE
                  CVALUE = LOBJECT(POSQ1+1:POSQ2-1)
               END IF
               GO TO 999
            ELSE
               CVALUE = LOBJECT(1:STRLEN(LOBJECT))
            END IF
         ENDIF
         GOTO 999
      ELSEIF (TYPE.EQ.' ') THEN
      END IF
  100 CONTINUE
C
C We don't recognize it : try to read it anyway.
C Try to read the value: try character string first
C
      IF (LOBJECT(1:1).EQ.'''') THEN
         POSQ1 = INDEX (LOBJECT, '''')
         POSQ2 = POSQ1 + INDEX (LOBJECT(POSQ1+1:), '''')
         IF ((POSQ1.GT.0).AND.(POSQ2.GT.POSQ1)) THEN
            IF (POSQ2.EQ.POSQ1+1) THEN
               CVALUE = ' '
            ELSE
               CVALUE = LOBJECT(POSQ1+1:POSQ2-1)
            END IF
            TYPE = 'C'
            GO TO 999
         END IF
      END IF
C
C By now we have eliminated a string so we can jettison everything
C beyond the /.
C
      POSSL = INDEX (LOBJECT, '/')
      IF (POSSL.GT.0) THEN
         LOBJECT = LOBJECT (1:POSSL-1)
      END IF
C
C Now try LOGICAL
C
      IF ((LOBJECT(1:1).EQ.'T').OR.(LOBJECT(1:1).EQ.'F')) THEN
         LVALUE = LOBJECT(1:1).EQ.'T'
         TYPE = 'L'
         GO TO 999
      END IF
C
C Now look for . to set if it is NOT an integer
C
      POSDOT = INDEX (LOBJECT, '.')
      IF (POSDOT.EQ.0) THEN
         CALL STRATOI (LOBJECT, STRLEN (LOBJECT), IVALUE)
         TYPE = 'I'
         GO TO 999
 200     CONTINUE
      ELSE 
C
C It now must be either Real, Double, or Complex
C
         POSE = INDEX (LOBJECT, 'E')
         IF (POSE.EQ.0) THEN
            GO TO 500
         ELSE
            IF ((POSE-POSDOT).LT.LREAL) THEN
               CALL STRATOF (LOBJECT, STRLEN (LOBJECT),
     1            RVALUE)
               TYPE = 'R'
               GO TO 999
            ELSE
               CALL STRATOD (LOBJECT, STRLEN (LOBJECT),
     1            DVALUE)
               TYPE = 'D'
               GO TO 999
            END IF
         END IF
      END IF
C
C No translation possible, God knows what it is, try last ditch effort
C
 500  CONTINUE
      TYPE = 'R'
      CALL STRATOF (LOBJECT, STRLEN (LOBJECT), RVALUE)
      GO TO 990
C
 600  CONTINUE
      TYPE = ' '
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
 1000 FORMAT (BN, L2)
 1001 FORMAT (A)
      END
