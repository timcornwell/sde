C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filtblsr.f	1.3    11/7/90
C
      SUBROUTINE FILTBLSR (NAME, TBLLIST)
C
CD READ tables from file with name NAME
C
C
C	NAME		CH*(*)	input	NAME of file as specified to user
C	TBLLIST	CH*(*)	input	List of tables to load
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, TBLLIST
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILTBLSR')
C
      CHARACTER		FILESYS*(SYSMXNAM)
      INTEGER		NDUMMY
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Now open file and read in binary data
C
      CALL DATGETC (NAME, 'FILSYS', FILESYS, 1, NDUMMY)
      IF (FILESYS.EQ.'FTS') THEN
         CALL FTSTBLSR(NAME, TBLLIST)
      ELSE
         MESSAGE = 'File system: '//FILESYS//' not supported'
         CALL ERRREPOR (ERRWRGTP, ROUTINE, MESSAGE)
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
