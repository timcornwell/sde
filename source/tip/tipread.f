C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipread.f	1.1    3/26/93
C
      SUBROUTINE TIPREAD (DBSUB)
C
CD Look up file name, read, put data into RAWDATA
C
C	DBSUB	CH*(*)	inp	Tipper DataBase Subdir
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	23 Dec 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	DBSUB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPREAD')
C
      CHARACTER*(SYSMXNAM)	TIPFILE
      CHARACTER*132		LINE
      LOGICAL			EOF
      INTEGER			NCHAR, I, ADD, NDUMMY
      REAL			VALUE
C
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETC (DBSUB, 'NewFileName', TIPFILE, 1, NDUMMY)
      CALL TXTOPEN (ROUTINE, TIPFILE, 'READ')
      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GOTO 999
      IF (LINE(1:3) .EQ. 'Cal') CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      CALL DATMAKAR (STRM2(DBSUB, 'RAWDATA'), 1, 1024, 'R', ADD)
C
      READ (LINE, *) VALUE
      MEMR(ADD) = VALUE
      DO 100 I = 1, 1023
         CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
         IF (EOF) THEN
            CALL MSGPUT ('Error reading tipper file '//TIPFILE, 'E')
            CALL MSGPUT ('Deleting appropriate subdirectory', 'E')
            CALL DATDELET (DBSUB)
            GOTO 900
         ENDIF
         READ (LINE, *) VALUE
         MEMR(ADD + I) = VALUE
 100  CONTINUE
 900  CONTINUE
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
