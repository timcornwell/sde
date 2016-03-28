C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipedit.f	1.1    3/26/93
C
      SUBROUTINE TIPEDIT (DB, THRESH, STIME)
C
CD Edit Raw tipper data based on statistics
C
C	DB	CH*(*)	inp	Tipper DataBase
C	THRESH	REAL	inp	Threshhold in Sigma (10?)
C	STIME	INT(2)	inp	Start and Stop Times
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
      CHARACTER*(*)	DB
      REAL		THRESH
      INTEGER		STIME(2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPEDIT')
C
      CHARACTER*(SYSMXNAM)	ANAME
      REAL			FRAC
      INTEGER	NAX, NAXIS(SYSMXDIM), ADD, I,
     $   	NFIX, MFIX, MTOT
      CHARACTER*1	TYPE
C      
      CHARACTER*(SYSMXNAM)	STRM2, STRINT
      INTEGER		I1, I2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL TIPRANGE (DB, STIME, I1, I2)
      CALL DATGETAR (STRM2(DB, 'LIST'), NAX, NAXIS, TYPE, ADD)
C
      MFIX = 0
      MTOT = 0
      DO 100 I = I1, I2
         MTOT = MTOT + 1
         ANAME = STRM2(DB, STRINT(MEMI(ADD+I)))
         CALL TIPREAD (ANAME)
         CALL TIPEDFIX (STRM2(ANAME, 'RAWDATA'), THRESH, NFIX)
         IF (NFIX .GT. 0) THEN
            MFIX = MFIX + 1
            WRITE (MESSAGE, 1234) NFIX, ANAME
 1234       FORMAT ('Edited ',I4,' raw points from ', A)
            CALL MSGPUT (MESSAGE, 'I')
            CALL TIPRENAM (ANAME)
            CALL TIPWRITE (ANAME)
         ENDIF
         CALL DATDELET (STRM2(ANAME, 'RAWDATA'))
 100  CONTINUE
      IF (MTOT .NE. 0) THEN
         FRAC = 100.0 * FLOAT(MFIX) / FLOAT(MTOT)
         WRITE (MESSAGE, 4321) FRAC
 4321    FORMAT ('Edited ', F7.3, '% of the files in range')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
       CALL MSGPUT ('Error Editing: no files in range', 'E')
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
