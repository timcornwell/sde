C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filsysrt.f	1.1    12/26/91
C
      SUBROUTINE FILSYSRT (FILENAME, FILEROOT)
C
C Extract root filename from filename.  (Everything but extension.)
C
C	FILENAME	CH*(*)	input	Filename
C	FILEROOT	CH*(*)	output	Root filename
C                                       (may be same as input)
C Audit trail:
C       Cloned from FILSYSEX
C                               D.S.Briggs      Aug 22 1991
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	FILENAME, FILEROOT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILSYSEX')
C
      CHARACTER*(SYSMXNAM)	BUFF
      INTEGER		I, L, STRLEN
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL STRUC (FILENAME, BUFF)
      L = STRLEN (BUFF)
      I = L + 1
C
      IF (INDEX(BUFF,'@').GT.0) THEN
 10      I = I - 1
         IF (BUFF(I:I).NE.'@') GO TO 10
      ELSE IF (INDEX(BUFF,'.').GT.0) THEN
 20      I = I - 1
         IF (BUFF(I:I).NE.'.') GO TO 20
      ELSE
         I = 0
      END IF
C
      IF ((BUFF(I+1:).EQ.'FTS').OR.(BUFF(I+1:).EQ.'SDE')) THEN
         IF (I.EQ.1) THEN
            BUFF = ' '
         ELSE
            BUFF = FILENAME(1:I-1)
         END IF
      ELSE
         BUFF = FILENAME
      END IF
      FILEROOT = BUFF
C
 999  CONTINUE
      END
