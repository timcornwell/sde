
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mosdog.f	1.1	 5/1/95
C
      SUBROUTINE SDEMAIN
C
CD Program to select just a subset of mosaic pointings
C
C Audit trail:
C	New task
C				M.A. Holdaway   May 1 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSDOG')
C
      CHARACTER*(SYSMXNAM)	INMOSFIL, OUTMOSFI, LISTFILE
      INTEGER		IP(100)
      CHARACTER*6	STRINT
      CHARACTER*(SYSMXNAM)	MODE, MOSS1, MOSS2, STRM2
      INTEGER		NDUMMY, NPC, NPCNEW, IPC, IPC2, NPCOUT
      CHARACTER*132	LINE
      LOGICAL		EOF
      INTEGER		NCHAR, STRLEN
C==================================================================
      CALL MSGWELCO ('I concatenate mosaic visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('In', INMOSFIL, 1, NDUMMY)
      CALL USRGETC('Out', OUTMOSFI, 1, NDUMMY)
      CALL USRGETC('ListFile', LISTFILE, 1, NDUMMY)
      CALL USRGETI('Pointings', IP, 100, NDUMMY)
      CALL USRGETC('Mode', MODE, 1, NDUMMY)
C
      CALL VISMOSGE ('Mos', INMOSFIL)
      CALL DATGETI ('Mos', 'NPC', NPC, 1, NDUMMY)
      CALL DATCREAT ('NewMos')
C
      IF (LISTFILE .NE. ' ') THEN
         NPCNEW = 0
         CALL TXTOPEN (ROUTINE, LISTFILE, 'READ')
 1       CONTINUE
            CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
            IF (ERROR) GOTO 999
            IF (EOF) GO TO 50
            IF (LINE(1:1).EQ.'#') GO TO 1
            NPCNEW = NPCNEW + 1
            IF (NPCNEW .GT. 100) THEN
               CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $              'More than 100 pointings in '//LISTFILE)
               GOTO 999
            ENDIF
            READ (LINE(:STRLEN(LINE)), *, ERR = 200) IP(NPCNEW)
         GOTO 1
 50      CONTINUE
         CALL TXTCLOSE (ROUTINE)
      ELSE
         DO 2 IPC = 1, 100
            IF (IP(IPC) .EQ. 0) THEN
               NPCNEW = IPC - 1
               GOTO 5
            ENDIF
 2       CONTINUE
 5       CONTINUE
      ENDIF
C
      IF (NPCNEW .GT. NPC) THEN
         WRITE (MESSAGE, 1043) NPC, NPCNEW
 1043    FORMAT('Infile has ',I3,' pointings, requesting ',I3)
         CALL ERRREPOR (ERRLOGIC, ROUTINE, MESSAGE)
         GOTO 999
      ENDIF
C
      IF (MODE .EQ. 'IN') THEN
         CALL MSGPUT ('Extracting pointings IN list', 'I')
         DO 100 IPC = 1, NPCNEW
            MOSS1 = STRM2 ('Mos', 'PC'//STRINT(IP(IPC)))
            MOSS2 = STRM2 ('NewMos', 'PC'//STRINT(IPC))
            CALL DATRENAM (MOSS1, MOSS2)
            WRITE (MESSAGE, 1198) MOSS1(:STRLEN(MOSS1)), 
     $           MOSS2(:STRLEN(MOSS2))
 1198       FORMAT ('Old pointing: ',A, '   to New pointing: ',A)
            CALL MSGPUT (MESSAGE, 'I')
 100     CONTINUE
         CALL DATPUTI ('NewMos', 'NPC', NPCNEW, 1)
      ELSE IF (MODE .EQ. 'OUT') THEN
         CALL MSGPUT ('Extracting pointings OUT of list', 'I')
         NPCOUT = 0
         DO 150 IPC = 1, NPC
            DO 140 IPC2 = 1, NPCNEW
               IF (IPC .EQ. IP(IPC2)) GOTO 150
 140        CONTINUE
            NPCOUT = NPCOUT + 1
            MOSS1 = STRM2 ('Mos', 'PC'//STRINT(IPC))
            MOSS2 = STRM2 ('NewMos', 'PC'//STRINT(NPCOUT))
            WRITE (MESSAGE, 1198) MOSS1(:STRLEN(MOSS1)), 
     $           MOSS2(:STRLEN(MOSS2))
            CALL MSGPUT (MESSAGE, 'I')
            CALL DATRENAM (MOSS1, MOSS2)
 150     CONTINUE
         CALL DATPUTI ('NewMos', 'NPC', NPCOUT, 1)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $        'Unknown mode: '//MODE)
         GOTO 999
      ENDIF
C
      CALL VISMOSPU ('NewMos', OUTMOSFI)
      GOTO 999
C
 200  CONTINUE
      CALL ERRREPOR (ERRBDARG, ROUTINE,
     $     'Error reading from text list: '//LINE)
      GOTO 999
C
 999  CONTINUE
      END
