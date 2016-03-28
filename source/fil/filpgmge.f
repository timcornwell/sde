C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filpgmge.f	1.3    7/18/97
C
      SUBROUTINE FILPGMGE (IMG, PGMFILE)
C
CD Get PGM style pixel values into an existing image
C
C	IMG	CH*(*)	input	Name of image to be filled
C	PGMFILE	CH*(*)	input	Name of PGM file
C
C Audit trail:
C	Original version
C				D.S.Briggs	15 Oct 1992
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	IMG, PGMFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILPGMGE')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD
      CHARACTER*1	ATYPE
C
      INTEGER		NROW, NCOL, IR, IC, IADD
      REAL		MAXGREY
      DOUBLE PRECISION	VALUE
      CHARACTER*30	TOKEN
      LOGICAL		EOF
C
      INTEGER		CRDRNAX, STRLEN
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IMG, NAX, NAXIS, ATYPE, ADD)
C
      CALL TXTOPEN ('PGM', PGMFILE, 'READ')
      IF (ERROR) GO TO 990
      CALL MSGPUT ('Opening PGM file ' // PGMFILE(1:STRLEN(PGMFILE)) //
     $   ' for READ as PGM','I')

      CALL GETTOKEN ('PGM', TOKEN, EOF, .TRUE.)
      IF (ERROR) GO TO 990
      IF (TOKEN.NE.'P2') THEN
         CALL ERRREPOR (ERRWRGTP, ROUTINE, 'Not a PGM file')
         GO TO 999
      END IF

      CALL GETTOKEN ('PGM', TOKEN, EOF, .FALSE.)
      IF (ERROR) GO TO 990
      READ (TOKEN, *, ERR=100) NCOL
      GO TO 110
 100  CALL ERRREPOR (ERRBDARG, ROUTINE, 'Misformed number of columns')
      GO TO 999

 110  CONTINUE
      CALL GETTOKEN ('PGM', TOKEN, EOF, .FALSE.)
      IF (ERROR) GO TO 990
      READ (TOKEN, *, ERR=120) NROW
      GO TO 130
 120  CALL ERRREPOR (ERRBDARG, ROUTINE, 'Misformed number of rows')
      GO TO 999

 130  CONTINUE
      CALL GETTOKEN ('PGM', TOKEN, EOF, .FALSE.)
      IF (ERROR) GO TO 990
      READ (TOKEN, *, ERR=140) MAXGREY
      GO TO 150
 140  CALL ERRREPOR (ERRBDARG, ROUTINE, 'Misformed max-grey level')
      GO TO 999

 150  CONTINUE
      IF ((NAXIS(1).NE.NCOL).OR.(NAXIS(2).NE.NROW)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Image size and PGM file do not match')
         GO TO 999
      END IF

      IF (CRDRNAX(NAX,NAXIS).GT.2) THEN
         CALL MSGPUT (
     $      'More than 2 axes.  Only first plane will be filled','W')
      END IF

      IADD = ADD
      DO 240 IR = 1, NROW
         DO 230 IC = 1, NCOL
            
            CALL GETTOKEN ('PGM', TOKEN, EOF, .FALSE.)
            IF (ERROR) GO TO 990
            IF (EOF) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Premature EOF')
               GO TO 999
            END IF

            READ (TOKEN, *, ERR=200) VALUE
            GO TO 220
 200        WRITE (MESSAGE, 210) IR, IC, TOKEN
 210        FORMAT ('Bad number.  R C # = ',I4,' ',I4,' ',A)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
            GO TO 999

 220        CONTINUE
            IADD = ADD + (NROW-IR)*NAXIS(1) + IC - 1
            IF (ATYPE.EQ.'R') THEN
               MEMR(IADD) = VALUE
            ELSE IF (ATYPE.EQ.'D') THEN
               MEMD(IADD) = VALUE
            ELSE IF (ATYPE.EQ.'I') THEN
               MEMI(IADD) = NINT(VALUE)
            ELSE
               CALL ERRREPOR (ERRWRGTP, ROUTINE, 'Array type ' // ATYPE
     $            // ' not supported')
               GO TO 999
            END IF

 230     CONTINUE
 240  CONTINUE
C
      CALL TXTCLOSE ('PGM')
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C
      SUBROUTINE GETTOKEN (HANDLE, TOKEN, EOF, FIRST)
C
C	HANDLE	CH*(*)	input	Handle to input file
C	TOKEN	CH*(*)	output	Token
C	EOF	CH*(*)	output	EOF on read
C	FIRST	LOG	input	Set to true on first call
C
C Audit trail:
C	Original version
C				D.S.Briggs	15 Oct 1992
C	Some stupid programs write PGM files without terminating
C	CR, which causes EOF to go true early.
C				D.S.Briggs	June 28 1994
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	HANDLE, TOKEN
      LOGICAL		EOF, FIRST
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GETTOKEN')
C
      CHARACTER		LINE*256
      LOGICAL		LASTEOF
      INTEGER		ISTART, IEND, LLEN
      SAVE		LINE, ISTART, LLEN, LASTEOF
C
      INTEGER		STRLEN
      DATA		ISTART/1/, LLEN/0/
C==================================================================
      IF (ERROR) GO TO 999
C
      IF (FIRST) LASTEOF = .FALSE.
      EOF = .FALSE.
      TOKEN = ' '
C
C Make sure that we have a line to work on
C
 100  CONTINUE
      IF (ISTART.GT.LLEN) THEN
         ISTART = 1
         IF (LASTEOF) THEN
            EOF = .TRUE.
            GO TO 999
         END IF
         CALL TXTREAD (HANDLE, LINE, LLEN, LASTEOF)
         LLEN = STRLEN (LINE)
         IF (ERROR) GO TO 990
         IF (LASTEOF.AND.(LLEN.EQ.0)) THEN
            EOF = .TRUE.
            GO TO 999
         END IF
      END IF
C
C Trim off leading whitespace
C
 200  CONTINUE
      IF (ISTART.GT.LLEN) GO TO 100
      IF (ICHAR(LINE(ISTART:ISTART)).LE.ICHAR(' ')) THEN
         ISTART = ISTART + 1
         GO TO 200
      END IF
C
C Starts with a comment is special case
C
      IF (LINE(ISTART:ISTART).EQ.'#') THEN
         ISTART = LLEN + 1
         GO TO 100
      END IF
C
C Scan down token looking for end or whitespace or comment
C
      IEND = ISTART + 1
 300  CONTINUE
      IF (IEND.GT.LLEN) GO TO 500
      IF (ICHAR(LINE(IEND:IEND)).LE.ICHAR(' ')) GO TO 500
      IF (LINE(IEND:IEND).EQ.'#') GO TO 500
      IEND = IEND + 1
      GO TO 300
C
C We have a token -- go home
C
 500  CONTINUE
      TOKEN = LINE(ISTART:IEND-1)
      ISTART = IEND
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
      

