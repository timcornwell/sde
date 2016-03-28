C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filparse.f	1.1    12/26/91
C
      SUBROUTINE FILPARSE (FILENAME, FILEROOT, FILEEXT, FILESYS)
C
C Parse filename to root and extension.
C
C	FILENAME	CH*(*)	input	Filename
C	FILEROOT	CH*(*)	output	Root filename
C       FILEEXT         CH*(*)  output  True filename extension
C       FILESYS         CH*(*)  output  'FTS' or 'SDE'
C                             (any output may be same as input)
C Audit trail:
C       Cloned from FILSYSEX
C                               D.S.Briggs      Aug 22 1991
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	FILENAME, FILEROOT, FILEEXT, FILESYS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILSYSEX')
C
      CHARACTER*(SYSMXNAM)	BUFFN, BUFFU
      INTEGER		I, L, STRLEN
C=======================================================================
      IF (ERROR) GO TO 999
C
      BUFFN = FILENAME
      CALL STRUC (BUFFN, BUFFU)
      L = STRLEN (BUFFN)
      I = L + 1
C
      IF (INDEX(BUFFU,'@').GT.0) THEN
 10      I = I - 1
         IF (BUFFU(I:I).NE.'@') GO TO 10
      ELSE IF (INDEX(BUFFU,'.').GT.0) THEN
 20      I = I - 1
         IF (BUFFU(I:I).NE.'.') GO TO 20
      ELSE
         I = 0
      END IF
C
      IF ((BUFFU(I+1:).EQ.'FTS').OR.(BUFFU(I+1:).EQ.'SDE')) THEN
         IF (I.LE.1) THEN
            FILEROOT = ' '
            FILEEXT = BUFFN(I+1:)
            FILESYS = BUFFU(I+1:)
         ELSE
            FILEROOT = BUFFN(1:I-1)
            FILEEXT = BUFFN(I+1:)
            FILESYS = BUFFU(i+1:)
         END IF
      ELSE
         FILEROOT = BUFFN
         FILEEXT = ' '
         FILESYS = 'FTS'
      END IF
C
 999  CONTINUE
      END
