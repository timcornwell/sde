C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)dump.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to DUMP a File
C
C Audit trail:
C	Now dumps history as well
C				T.J.Cornwell	Jan 11 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'DUMP')
C
      INTEGER 		NDUMMY
      EXTERNAL		DAIDUMP
      CHARACTER*(SYSMXNAM) 	INFILE
C==================================================================
C
      CALL MSGWELCO ('I dump files')
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Read file.
C
      CALL USRGETC ('Filename', INFILE, 1, NDUMMY)
      CALL FILIMGGE ('File', INFILE, ' ')
C
C Dump directory
C 
      CALL MSGPUT ('Dump of directory structure:', 'I')
      CALL DATDUMP('File')
C
C Dump history cards
C
      CALL MSGPUT (' ', 'I')
      CALL MSGPUT ('History cards:', 'I')
      CALL HISLIST ('File')
C
C Dump header info
C
      CALL MSGPUT (' ', 'I')
      CALL MSGPUT ('Header:', 'I')
      CALL CRDLIST ('File')
C
      END
