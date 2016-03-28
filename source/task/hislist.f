C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hislist.f	1.1    6/24/93
C
      SUBROUTINE SDEMAIN
C
CD Program to print the history of SDE files or FITS images
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R. G. Marson 24th June 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HISLIST')
C
C Function Declarations
C   
C
C Local Variables
C
      CHARACTER*(SYSMXNAM)	FILENAME, FILESYS, INTYPE
      INTEGER		NDUMMY
C
C==================================================================
C
      CALL MSGWELCO ('I print the history records')
      CALL USRCTL
C
C Get input image
C
      CALL USRGETC ('InFile', FILENAME, 1, NDUMMY)
      CALL FILSYSEX (FILENAME, FILESYS)
C
C If it is an SDE file just load it.
C
      IF (FILESYS(1:3).EQ.'SDE') THEN
         CALL DATCREAT('DATA')
         CALL DATREAD('DATA', FILENAME)
      ELSE IF(FILESYS(1:3).EQ.'FTS') THEN
         CALL USRGETC ('InType', INTYPE, 1, NDUMMY)
         CALL STRUC(INTYPE, INTYPE)
C
C This program can also do FITS images (and groups files)
C
         IF (INTYPE(1:3).EQ.'GRP') THEN
            CALL FILGRPGE('DATA', FILENAME, '*', ' ', FILESYS)
         ELSE
            CALL FILIMGGE('DATA', FILENAME, ' ')
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE, 'Unable to load file')
         GOTO 999
      END IF
C
C Print the history records.
C
      CALL HISLIST('DATA')
C
 999  CONTINUE
      END
