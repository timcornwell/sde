C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filimmpu.f	1.1    12/7/90
C
      SUBROUTINE FILIMMPU (NAME, FILENAME, NFILES, TBLS)
C
CD Writes out a group of image files 
C
C	NAME	CH*(*)	input	Base Name of directory entry
C	FILENAME CH*(*)	input	Base filename for files
C	NFILES	CH*(*)	input	Number of files to write out
C	TBLS	CH*(*)	input	Tables to write out
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Nov 6 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input declarations
C
      INTEGER           NFILES
      CHARACTER*(*)	NAME, FILENAME, TBLS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILIMMPU')
C
C Function Declarations
C
      CHARACTER*6 STRINT
      INTEGER STRLEN
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) FILSTR, NAMSTR
      CHARACTER*6 SEQSTR
      INTEGER IMG, DOTPOS, NAMLEN
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Position of '.' in filename (so the extension is preserved)
C
      DOTPOS = INDEX(FILENAME, '.')
      NAMLEN = STRLEN(NAME)
C
C Loop over each entry
C
      DO IMG = 0, NFILES - 1
C
C Generate output filename (FILEn.xxx or FILEn)
C
         IF (DOTPOS.GT.0) THEN
            SEQSTR = STRINT(IMG)
            FILSTR = FILENAME(1:DOTPOS-1)//
     $           SEQSTR(1:STRLEN(SEQSTR))//FILENAME(DOTPOS:)
         ELSE
            FILSTR = FILENAME(1:STRLEN(FILENAME))//STRINT(IMG)
         END IF
C
C Generate node name
C
         NAMSTR = NAME(1:NAMLEN)//STRINT(IMG)
C
C Save inputs in history file
C
         CALL HISINPUT (NAMSTR)
C
C Write the file
C
         CALL FILIMGPU(NAMSTR, FILSTR, TBLS)
      END DO
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
