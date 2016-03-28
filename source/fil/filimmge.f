C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filimmge.f	1.4    7/18/97
C
      SUBROUTINE FILIMMGE (NAME, FILENAME, TBLS, IGNORE, IMG)
C
CD Reads in multiple images as a group
C
C	NAME	CH*(*)	input	Base name of directory entry
C	FILENAME CH*(*)	input	Base name of filename
C	TBLS	CH*(*)	input	Tables to load
C	IGNORE	CH*(*)	input	Entries to ignore when checkin header
C	IMG	CH*(*)	output	Number of images to read/ actually read
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Nov 6 1990
C	Fixed String concatenations
C				T.J.Cornwell	Jan 6 1994
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input declarations
C
      CHARACTER*(*)	NAME, FILENAME, IGNORE, TBLS
      INTEGER           IMG
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILIMMGE')
C
C Function Declarations
C
      CHARACTER*6 STRINT
      INTEGER STRLEN
      LOGICAL FILEXIST
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) FILSTR, NAMSTR, STRTMP
      CHARACTER*6 SEQSTR
      INTEGER DOTPOS, MAXIMG
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DOTPOS = INDEX(FILENAME, '.')
      IF (IMG.LE.0) THEN 
         MAXIMG = SYSMXIMG
      ELSE
         MAXIMG =MIN(IMG, SYSMXIMG)
      END IF
C
C Loop over at most SYSMXIMG files
C
      DO 5 IMG = 0, MAXIMG - 1
C
C First generate input filename (FILEn.xxx or FILEn)
C
         IF (DOTPOS.GT.0) THEN
            SEQSTR = STRINT(IMG)
            FILSTR = FILENAME(1:DOTPOS-1)//
     $           SEQSTR(1:STRLEN(SEQSTR))//FILENAME(DOTPOS:)
         ELSE
            FILSTR = FILENAME(1:STRLEN(FILENAME))//STRINT(IMG)
         END IF
C
C Check if it exists. If not then exit. (A poor mans do while)
C
            IF (FILEXIST(FILSTR)) THEN
C
C Get file
C
               NAMSTR = NAME
               CALL STRAPPEN (NAMSTR, STRINT(IMG))
               CALL FILIMGGE (NAMSTR, FILSTR, TBLS)
               IF (IMG.GT.0) THEN
                  STRTMP = NAME
                  CALL STRAPPEN (STRTMP, '0')
                  CALL CRDCHKIG(STRTMP, NAMSTR,  IGNORE)
               END IF
            ELSE
               GO TO 990
            END IF
 5       CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
