C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filsysex.f	1.6   12/11/92
C
      SUBROUTINE FILSYSEX (FILENAME, FILESYS)
C
C Extract filesys from filename
C
C
C	FILENAME	CH*(*)	input	Filename
C	FILESYS		CH*(*)	output	File system e.g. FTS for FITS
C                                       (may be same as input)
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added @ searching i.e. name can now be 3C10MAP@SDE
C				Your name	Date
C       Now searches for the *last* period in a name.  That is, the
C       name 3c84.clean.sde will return 'SDE', not the default (which
C       is 'FTS').  The comparison for the extension is now case
C       insensitive as well.
C                               D.S.Briggs      Sep 10 1990
C       Oops!  Upper case translation didn't work.  Fixed.  Reordered
C       logic so that FILESYS can be FILENAME, if desired.
C                               D.S.Briggs      Aug 23 1991
C	Added support for PGM files.
C				D.S.Briggs	Oct 29 1992
C------------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	FILENAME, FILESYS
C
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
         FILESYS = BUFF(I+1:)
      ELSE IF (INDEX(BUFF,'.').GT.0) THEN
 20      I = I - 1
         IF (BUFF(I:I).NE.'.') GO TO 20
         FILESYS = BUFF(I+1:)
      ELSE
         FILESYS = ' '
      END IF
C
      IF ((FILESYS.NE.'FTS').AND.
     $    (FILESYS.NE.'SDE').AND.
     $    (FILESYS.NE.'PGM')) THEN
         FILESYS = 'FTS'
      END IF
C
 999  CONTINUE
      END
