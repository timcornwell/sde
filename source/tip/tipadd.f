C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipadd.f	1.1    3/26/93
C
      SUBROUTINE TIPADD (DB, TIPFILE)
C
CD Add a TIPPER stability file to the DataBasen
C
C	DB	CH*(*)	inp	Tipper DataBase
C	TIPFILE	CH*(*)	inp	Name of Tipper File
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	22 Dec 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	DB, TIPFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPADD')
C
      INTEGER	NAX, NAXIS(SYSMXDIM), ADD, ADD2, NL, IL, NCHAR
      CHARACTER*1	TYPE
      CHARACTER*(SYSMXNAM)	MYNAME, MYNAME2, TEMPNAME
      CHARACTER*132	LINE
      LOGICAL	EXISTS, EOF, CALIBRUN
C
      INTEGER	STRLEN, NDUMMY, DATADD, INAME
      CHARACTER*(SYSMXNAM)	STRM2
      CHARACTER*(10)	STRINT
      LOGICAL	DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Is the file there?  Is it a calibrator?
C
      CALL TXTOPEN (ROUTINE, TIPFILE, 'READ')
      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF(ERROR) THEN
         CALL MSGPUT ('Tipper File Doesn''t exist: '//TIPFILE, 'E')
         GOTO 990
      ENDIF
      IF (LINE(1:3) .EQ. 'Cal') THEN
         CALIBRUN = .TRUE.
      ELSE
         CALIBRUN = .FALSE.
      ENDIF         
      CALL TXTCLOSE(ROUTINE)
C
C Distill Subdirectory Name from TIPFILE
C
      MYNAME2 = TIPFILE
      NL = STRLEN(TIPFILE)
      DO 100 IL = NL, 1, -1
         IF (TIPFILE(IL:IL) .EQ. '/') THEN
            MYNAME2 = TIPFILE(IL+1:NL)
            GOTO 200
         ENDIF
 100  CONTINUE
 200  CONTINUE
      IF (INDEX(MYNAME2, '.') .NE. 0) THEN
         MYNAME = MYNAME2(1:INDEX(MYNAME2, '.')-1)
      ELSE
         MYNAME = MYNAME2
      ENDIF
C
      READ(MYNAME, *) INAME
C
      IF (.NOT. DATEXIST(DB)) THEN
         CALL DATCREAT(DB)
         CALL DATMAKAR (STRM2(DB, 'LIST'), 1, 1, 'I', ADD)
         MEMI(ADD) = INAME
      ELSE
         CALL DATGETAR (STRM2(DB, 'LIST'), NAX, NAXIS, TYPE, ADD)
         EXISTS = .FALSE.            
         DO 232 IL = 0, NAXIS(1) - 1
            IF (INAME .EQ. MEMI(ADD + IL)) THEN
               CALL MSGPUT ('Tipper data already cataloged: '//MYNAME,
     $            'W')
               EXISTS = .TRUE.
               GOTO 233
            ENDIF
 232     CONTINUE
 233     CONTINUE
         IF (.NOT. EXISTS) THEN
            CALL DATMAKAR (STRM2(DB, 'LIST2'), 1, (NAXIS(1)+1), 'I',
     $         ADD2)
            CALL PIXICOPY (MEMI(ADD), 1, MEMI(ADD2), 1, NAXIS(1))
            MEMI(ADD2 + NAXIS(1)) = INAME
            CALL DATDELET (STRM2(DB, 'LIST'))
            CALL ARRCOPY (STRM2(DB, 'LIST2'), STRM2(DB, 'LIST')) 
            CALL UTLISORT(NAXIS(1)+1, MEMI(DATADD(STRM2(DB, 'LIST2'))),
     $         MEMI(DATADD(STRM2(DB, 'LIST'))) )
            CALL DATDELET (STRM2(DB, 'LIST2'))
         ENDIF            

      ENDIF
C
      IF (EXISTS) THEN
C
C See if we need to give a new filename
C
         CALL DATGETC (STRM2(DB, STRINT(INAME)), 'NewFileName', 
     $      TEMPNAME, 1, NDUMMY)
         IF (TEMPNAME .NE. TIPFILE) THEN
            CALL DATDELET(STRM2(DB, STRINT(INAME))) 
            CALL DATCREAT (STRM2(DB, STRINT(INAME))) 
            CALL DATPUTC (STRM2(DB, STRINT(INAME)), 'OriginalFileName',
     $         TIPFILE,1)
            CALL DATPUTC (STRM2(DB, STRINT(INAME)), 'NewFileName', 
     $         TIPFILE, 1)
         ENDIF
      ELSE
         CALL DATCREAT (STRM2(DB, STRINT(INAME))) 
         CALL DATPUTC (STRM2(DB, STRINT(INAME)), 'OriginalFileName', 
     $      TIPFILE,1)
         CALL DATPUTC (STRM2(DB, STRINT(INAME)), 'NewFileName', 
     $      TIPFILE, 1)
      ENDIF
      CALL DATPUTL (STRM2(DB, STRINT(INAME)), 'Calibrator', CALIBRUN, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
