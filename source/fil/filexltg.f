C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filexltg.f	1.2    10/7/93
C
C Audit trail:
      SUBROUTINE FILEXLTG (DIR, EXLFILE)
C
CD Get arbitrary table from 'Excel' file
C
C	DIR	CH*(*)	input	Directory to put columns
C	EXLFILE	CH*(*)	input	Name of input file
C
C	The EXL file has a two line header containing the names of the
C	columns and their type.  The data is read into an array of the
C	same name, stored in DIR.
C
C	The format is primarily designed to be used with the curvefitting
C	program GaussFit, but it is a simple ASCII interchange format that
C	should be understood by most spreadsheet type programs
C
C Audit trail:
C	Original version:
C				D.S.Briggs	Sept 16 1993
C	Added filename output
C				D.S.Briggs	Oct 4 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILEXLTG')
C
      CHARACTER*(*)	DIR, EXLFILE
C
      INTEGER		MAXCOLS
      PARAMETER		(MAXCOLS=30)
C
      CHARACTER*1	ATYPE(MAXCOLS)
      INTEGER		AADD(MAXCOLS)
C
      INTEGER		NROW, NCOL, T1, T2, IROW, ICOL, NCHAR
      CHARACTER*(SYSMXNAM)	TMPSTR
      CHARACTER		TAB*1, CTYPE
      LOGICAL		EOF, DODEF
C
      INTEGER		STRLEN
      CHARACTER*(SYSMXNAM)	STRM2
C==================================================================
      IF (ERROR) GO TO 999
C
C Write message to user
C
      WRITE (MESSAGE, 1000) EXLFILE(1:STRLEN(EXLFILE)), DIR
 1000 FORMAT ('Opening EXL table file ',A,' for READ into dir ',A)
      CALL MSGPUT (MESSAGE, 'I')
C
C Find number of rows and columns
C
      TAB = CHAR(9)
      CALL TXTOPEN (ROUTINE, EXLFILE, 'READ')
      IF (ERROR) GO TO 990
      CALL TXTREAD (ROUTINE, STRBUF, NCHAR, EOF)
      CALL TXTREAD (ROUTINE, STRBUF, NCHAR, EOF)
      IF (ERROR) GO TO 990
      IF (EOF) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Premature EOF')
         GO TO 999
      END IF
C
C First the columns      
C
      NCOL = 0
      T1 = 0
 100  CONTINUE
      T2 = INDEX(STRBUF(T1+1:),TAB) + T1
      IF (T2.EQ.T1) T2 = LEN(STRBUF)
      IF (STRBUF(T1+1:).NE.' ') THEN
         IF (T2.LE.T1+1) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Null column type')
            GO TO 999
         END IF
         IF (STRBUF(T1+1:T2-1).EQ.'double') THEN
            CTYPE = 'D'
         ELSE IF (STRBUF(T1+1:T2-1).EQ.'char') THEN
            CTYPE = 'C'
         ELSE
            CALL ERRREPOR (ERRFATAL, ROUTINE,
     $         'Only column types double or char supported')
            GO TO 999
         END IF
         NCOL = NCOL + 1
         IF (NCOL.GT.MAXCOLS) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Too many columns')
            GO TO 999
         END IF
         ATYPE(NCOL) = CTYPE
         T1 = T2
         GO TO 100
      END IF
C
C Now the rows
C
      NROW = 0
 110  CONTINUE
      CALL TXTREAD (ROUTINE, STRBUF, NCHAR, EOF)
      IF (ERROR) GO TO 990
      IF (EOF) GO TO 120
      IF (NCHAR.EQ.0) GO TO 110
      NROW = NROW + 1
      GO TO 110
 120  CONTINUE
      CALL TXTCLOSE (ROUTINE)      
      IF (ERROR) GO TO 990
      NROW = NROW
      WRITE (MESSAGE, 1120) NROW, NCOL
 1120 FORMAT ('Found ',I6,' rows and',I3,' columns')
      CALL MSGPUT (MESSAGE, 'I')
C
C Parse columns and make arrays
C
      CALL TXTOPEN (ROUTINE, EXLFILE, 'READ')
      IF (ERROR) GO TO 990
      CALL TXTREAD (ROUTINE, STRBUF, NCHAR, EOF)
C
      T1 = 0
      ICOL = 0
 200  CONTINUE
      T2 = INDEX(STRBUF(T1+1:),TAB) + T1
      IF (T2.EQ.T1) T2 = LEN(STRBUF)
      IF (STRBUF(T1+1:).NE.' ') THEN
         ICOL = ICOL + 1
         IF (ICOL.GT.MAXCOLS) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Too many columns')
            GO TO 999
         END IF
         IF (ICOL.GT.NCOL) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Columns not consistent')
            GO TO 999
         END IF
         IF (T2.LE.T1+1) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Null column name')
            GO TO 999
         END IF
         IF (DIR.EQ.' ') THEN
            TMPSTR = STRBUF(T1+1:T2-1)
         ELSE
            TMPSTR = STRM2(DIR,STRBUF(T1+1:T2-1))
         END IF
         IF (ATYPE(ICOL).EQ.'D') THEN
            CALL DATMAKAR (TMPSTR, 1, NROW, 'D', AADD(ICOL))
         ELSE IF (ATYPE(ICOL).EQ.'C') THEN
            CALL DATMAKAR (TMPSTR, 1, NROW, 'S', AADD(ICOL))
         END IF
         T1 = T2
         GO TO 200
      END IF
      IF (ICOL.NE.NCOL) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Columns not consistent')
         GO TO 999
      END IF
C
C Now actually read in the data
C      
      CALL TXTREAD (ROUTINE, STRBUF, NCHAR, EOF)
      IF (ERROR) GO TO 990
      DO 250 IROW = 1, NROW
 230     CONTINUE
         CALL TXTREAD (ROUTINE, STRBUF, NCHAR, EOF)
         IF (NCHAR.EQ.0) GO TO 230
         IF (EOF) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Premature EOF')
            GO TO 999
         END IF
         T1 = 0
         DO 240 ICOL = 1, NCOL
            T2 = INDEX(STRBUF(T1+1:),TAB) + T1
            IF (T2.EQ.T1) T2 = LEN(STRBUF)
            DODEF = T2.LE.T1+1
            IF (.NOT.DODEF)
     $         DODEF = DODEF .OR. (STRBUF(T1+1:T2-1).EQ.' ')
            
            IF (DODEF) THEN
C					Default to previous value
               IF (IROW.EQ.1) THEN
                  CALL ERRREPOR (ERRFATAL, ROUTINE,
     $               'Can''t default on first row')
                  GO TO 999
               ELSE
                  IF (ATYPE(ICOL).EQ.'D') THEN
                     MEMD(AADD(ICOL)+IROW-1) = MEMD(AADD(ICOL)+IROW-2)
                  ELSE IF (ATYPE(ICOL).EQ.'C') THEN
                     MEMC(AADD(ICOL)+IROW-1) = MEMC(AADD(ICOL)+IROW-2)
                  END IF
               END IF
            ELSE
               IF (ATYPE(ICOL).EQ.'D') THEN
                  READ (STRBUF(T1+1:T2-1), *, ERR=235)
     $               MEMD(AADD(ICOL)+IROW-1)
                  GO TO 237
 235              CONTINUE
                  WRITE (MESSAGE, 1235) IROW, ICOL
 1235             FORMAT ('Malformed number R=',I5,'  C=',I3)
                  CALL ERRREPOR (ERRFATAL, ROUTINE, MESSAGE)
                  GO TO 999
 237              CONTINUE
               ELSE IF (ATYPE(ICOL).EQ.'C') THEN
                  MEMC(AADD(ICOL)+IROW-1) = STRBUF(T1+1:T2-1)
               END IF
            END IF
            T1 = T2
 240     CONTINUE
 250  CONTINUE
C
C All done
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
