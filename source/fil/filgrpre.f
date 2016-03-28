C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filgrpre.f	1.3    11/7/90
C
      SUBROUTINE FILGRPRE (NAME, PARLIST)
C
CD READ parameters from file with name NAME
C
C
C	NAME		CH*(*)	input	NAME of file as specified to user
C	PARLIST	CH*(*)	input	List of parameters to load
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, PARLIST
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILGRPRE')
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
         CALL FTSGRPRE(NAME, PARLIST)
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
