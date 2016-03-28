C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipstat.f	1.1    3/26/93
C
      SUBROUTINE TIPSTAT (DB, SFILE)
C
CD Prints out the status of a Tipper DataBase
C
C	DB	CH*(*)	inp	Tipper Data Base
C	SFILE	CH*(*)	inp	Status File ('/dev/tty' = SCREEN)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	22 DEC 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	DB, SFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPSTAT')
C
      CHARACTER*10	ANAME
      CHARACTER*3	CAL, EDIT
      CHARACTER*8	FIELD(6)
      CHARACTER*(SYSMXNAM)	NAME1, NAME2, COMMENT, DATE
      INTEGER	NAX, NAXIS(SYSMXDIM), ADD, N, I, J, NDUMMY
      INTEGER	IT
      CHARACTER*1	TYPE
      LOGICAL		CALIBRUN
      REAL		VALUE
      CHARACTER*132	LINE
C
      INTEGER	DATADD
      LOGICAL	DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      CHARACTER*15		STRINT
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (.NOT.DATEXIST(DB)) THEN
         CALL MSGPUT ('DataBase Doesn''t Exist: '//DB, 'E')
         GOTO 999
      ENDIF
      CALL TXTOPEN (ROUTINE, SFILE, 'WRITE')
      CALL TXTWRITE (ROUTINE, ' ')
      IF (ERROR) GOTO 999
      CALL TXTWRITE (ROUTINE, 'This file is '//SFILE)
      CALL MSGPUT ('Writing Status file to '//SFILE, 'I')
C
C Check for global messages
C
      DO 20 J = 0, 99
         IF (DATEXIST(STRM2(DB, 'COMMENT'//STRINT(J)))) THEN
            CALL DATGETC (DB, 'COMMENT'//STRINT(J),
     $         COMMENT, 1, NDUMMY)
            CALL DATGETC (DB, 'COMDATE'//STRINT(J),
     $         DATE, 1, NDUMMY)
            LINE = COMMENT(1:SYSMXNAM)//DATE(1:24)
            CALL TXTWRITE (ROUTINE, LINE)
         ELSE
            GOTO 30
         ENDIF
 20   CONTINUE
 30   CONTINUE
C
      MESSAGE = 'Date-Time  C  E    MIN     MAX     ASD      SSF  '
     $   //'   CASD     CSSF'
      CALL TXTWRITE (ROUTINE, ' ')
      CALL TXTWRITE (ROUTINE, MESSAGE)
C
      IT = 4
C
C IT = 4 correspnds to 56 s
C
      CALL DATGETAR (STRM2(DB, 'LIST'), NAX, NAXIS, TYPE, ADD)
      N = NAXIS(1)
      DO 100 I = 0, N-1
         ANAME = STRINT (MEMI(ADD + I) )
C
C
C Check for local messages
C
         IF (ERROR) GOTO 990
         DO 120 J = 0, 99
            IF (DATEXIST(STRM3(DB,  ANAME, 'COMMENT'//STRINT(J)))) THEN
               CALL DATGETC (STRM2(DB, ANAME), 'COMMENT'//STRINT(J),
     $            COMMENT, 1, NDUMMY)
               CALL ERRCANCE
               CALL DATGETC (STRM2(DB, ANAME), 'COMDATE'//STRINT(J),
     $            DATE, 1, NDUMMY)
               LINE = ANAME//COMMENT(1:SYSMXNAM)//DATE(1:24)
               CALL TXTWRITE (ROUTINE, LINE)
            ELSE
               GOTO 130
            ENDIF
 120     CONTINUE
 130     CONTINUE
C
         CALL DATGETL (STRM2(DB, ANAME), 'Calibrator', CALIBRUN, 1,
     $      NDUMMY)
         CAL = ' '
         IF (CALIBRUN) CAL = ' C '
C
         CALL DATGETC (STRM2(DB, ANAME), 'OriginalFileName', NAME1,
     $      1, NDUMMY)
         CALL DATGETC (STRM2(DB, ANAME), 'NewFileName', NAME2, 1,
     $      NDUMMY)
         EDIT = ' '
         IF (NAME1 .NE. NAME2) EDIT = ' E '
C
         IF (DATEXIST(STRM3(DB, ANAME, 'MIN'))) THEN
            CALL DATGETR (STRM2(DB, ANAME), 'MIN', VALUE, 1, NDUMMY)
            WRITE (FIELD(1), '(F8.3)') VALUE
         ELSE
            FIELD(1) = '    '
         ENDIF
C
         IF (DATEXIST(STRM3(DB, ANAME, 'MAX'))) THEN
            CALL DATGETR (STRM2(DB, ANAME), 'MAX', VALUE, 1, NDUMMY)
            WRITE (FIELD(2), '(F8.3)') VALUE
         ELSE
            FIELD(2) = '    '
         ENDIF
C
         IF (DATEXIST(STRM3(DB, ANAME, 'AV/ASD'))) THEN
            VALUE = MEMR(DATADD(STRM3(DB, ANAME, 'AV/ASD')) + IT)
            WRITE (FIELD(3), '(F8.3)') VALUE
         ELSE
            FIELD(3) = '    '
         ENDIF
C
         IF (DATEXIST(STRM3(DB, ANAME, 'SF/SSF'))) THEN
            VALUE = MEMR(DATADD(STRM3(DB, ANAME, 'SF/SSF')) + IT)
            WRITE (FIELD(4), '(F8.3)') VALUE
         ELSE
            FIELD(4) = '    '
         ENDIF
C
         IF (DATEXIST(STRM3(DB, ANAME, 'CAV/ASD'))) THEN
            VALUE = MEMR(DATADD(STRM3(DB, ANAME, 'CAV/ASD')) + IT)
            WRITE (FIELD(5), '(F8.3)') VALUE
         ELSE
            FIELD(5) = '    '
         ENDIF
C
         IF (DATEXIST(STRM3(DB, ANAME, 'CSF/SSF'))) THEN
            VALUE = MEMR(DATADD(STRM3(DB, ANAME, 'CSF/SSF')) + IT)
            WRITE (FIELD(6), '(F8.3)') VALUE
         ELSE
            FIELD(6) = '    '
         ENDIF
C

C
         MESSAGE = ANAME//CAL//EDIT//FIELD(1)//FIELD(2)//FIELD(3)//
     $      FIELD(4)//FIELD(5)//FIELD(6)
         CALL TXTWRITE (ROUTINE, MESSAGE)
C
 100  CONTINUE
      CALL TXTCLOSE (ROUTINE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
