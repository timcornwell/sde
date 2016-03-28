C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filarrpu.f	1.1    11/7/94
C
      SUBROUTINE FILARRPU (NAME, FILENAME)
C
CD Put array to simple binary file
C
C	NAME	 CH*(*)	input	NAME of array
C	FILENAME CH*(*)	input	File name
C
C This is mostly intended for exporting very large arrays to external
C programs.  Smaller arrays should consider using the EXL format.
C
C The basic file format has a single binary write of the total number
C of pixels, and then another write with the array values
C
C With the SM filecap entry
C unix1d|UNIX1D|Fortran 1-D unformatted files under Unix:\
C	:HS#-1:nx#0:ny#-1:RS#4:RL#-1:RE#4:
C this can be used to import binary arrays into SuperMongo
C
C Audit trail:
C	Original version
C				D.S.Briggs	Oct 30 1994
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILARRPU')
C
      CHARACTER		ATYPE*1
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD, NDUMMY,
     $   		RNAX, NT, I, LUN
C
      INTEGER		CRDRNAX, STRLEN, SYSGETLU
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, ADD)
      RNAX = CRDRNAX(NAX, NAXIS)
      NT = 1
      DO 10 I = 1, RNAX
         NT = NT * NAXIS(I)
 10   CONTINUE
C
      IF ((ATYPE.NE.'R').AND.(ATYPE.NE.'D').AND.(ATYPE.NE.'I')
     $    .AND.(ATYPE.NE.'L')) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array type not supported')
         GO TO 999
      END IF
C
      MESSAGE = 'Opening BINARY file ' // FILENAME(1:STRLEN(FILENAME))
     $   // ' for WRITE as ' // NAME
      CALL MSGPUT (MESSAGE, 'I')
      CALL FILDEL (FILENAME)
      IF (ERROR) GO TO 999
C
      LUN = SYSGETLU (NDUMMY)
      OPEN (UNIT=LUN, FILE=FILENAME, FORM='UNFORMATTED', STATUS='NEW')
C
      WRITE (LUN) NT
C
      IF (ATYPE.EQ.'R') THEN
         WRITE (LUN) (MEMR(ADD+I-1), I=1,NT)
      ELSE IF (ATYPE.EQ.'D') THEN
         WRITE (LUN) (MEMD(ADD+I-1), I=1,NT)
      ELSE IF (ATYPE.EQ.'I') THEN
         WRITE (LUN) (MEMI(ADD+I-1), I=1,NT)
      ELSE IF (ATYPE.EQ.'L') THEN
         WRITE (LUN) (MEML(ADD+I-1), I=1,NT)
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'I''m confused!')
         GO TO 999
      END IF
C
      CLOSE (UNIT=LUN)
      CALL SYSFRELU (LUN)
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
