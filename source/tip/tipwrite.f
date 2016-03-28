C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipwrite.f	1.1    3/26/93
C
      SUBROUTINE TIPWRITE (DBSUB)
C
CD Look up file name, writte RAWDATA to file
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
      PARAMETER		(ROUTINE = 'TIPWRITE')
C
      CHARACTER*(SYSMXNAM)	TIPFILE
      CHARACTER*132		LINE
      CHARACTER*1		TYPE
      LOGICAL			ISCAL
      INTEGER			I, ADD, NDUMMY,
     $   			NAX, NAXIS(SYSMXDIM)
C
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETC (DBSUB, 'NewFileName', TIPFILE, 1, NDUMMY)
      CALL TXTOPEN (ROUTINE, TIPFILE, 'WRITE')
      IF (ERROR) GOTO 999
      CALL DATGETAR (STRM2(DBSUB, 'RAWDATA'), NAX, NAXIS, TYPE, ADD)
      IF (ERROR) GOTO 990
C
      CALL DATGETL (DBSUB, 'Calibrator', ISCAL, 1, NDUMMY)
      IF (ISCAL) THEN
         CALL TXTWRITE (ROUTINE, 'Calibration Run')
      ENDIF
      DO 100 I = 0, 1023
         WRITE (LINE, *) MEMR(ADD + I)
         CALL TXTWRITE (ROUTINE, LINE)
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
