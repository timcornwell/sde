C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mos2vis.f	1.1	 9/23/91
C
      SUBROUTINE SDEMAIN
C
CD Program to list mosaic data
C
C Audit trail:
C	New task
C				M.A.Holdaway	Sep 23 1991
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOS2VIS')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, VISFILE
      CHARACTER*6	STRINT
      INTEGER		NDUMMY, IPC
      LOGICAL		DATEXIST
C==================================================================
      CALL MSGWELCO ('I list mosaic visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Mos', MOSFILE, 1, NDUMMY)
      CALL USRGETI ('Pointing', IPC, 1, NDUMMY)
      CALL USRGETC ('Visfile', VISFILE, 1, NDUMMY)
C
      CALL VISMOSGE ('Mos', MOSFILE)
C
      IF (DATEXIST ('Mos/PC'//STRINT(IPC))) THEN
         CALL VISPUT ('Mos/PC'//STRINT(IPC), VISFILE, 'OBS', 'I', '*',
     $      ' ')
      ELSE
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'Pointing '//STRINT(IPC)//
     $      ' not found in mosaic database')
         GOTO 999
      ENDIF
C
 999  CONTINUE
      END
