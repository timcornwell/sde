C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipdel.f	1.1    3/26/93
C
      SUBROUTINE TIPDEL (DB, STIME)
C
CD Delete database entries from STIME(1) to STIME(2)
C
C	DB	CH*(*)	inp	Tipper DataBase
C	STIME	INT(2)	inp	Start and Stop Times
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	27 Dec 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	DB
      INTEGER		STIME(2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPDEL')
C
      CHARACTER*(SYSMXNAM)	ANAME
      INTEGER	NAX, NAXIS(SYSMXDIM), ADD, I,
     $   	NDEL
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
      NDEL = 0
      DO 100 I = I1, I2
         NDEL = NDEL + 1
         ANAME = STRM2(DB, STRINT(MEMI(ADD+I)))
         CALL DATDELET (ANAME)
         CALL MSGPUT ('Deleted Subdirectory '//ANAME, 'W')
 100  CONTINUE
      WRITE (MESSAGE, 4321) NDEL
 4321 FORMAT ('Deleted ',I4,' Subdirectories')
      CALL MSGPUT (MESSAGE, 'W')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
