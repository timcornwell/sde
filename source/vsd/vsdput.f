C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdput.f	1.1	 5/4/93
C
      SUBROUTINE VSDPUT (PDT, FILE)
C
CD Write out polarization D Terms
C
C	PDT	CH*(*)	inp	D Terms Database
C	FILE	CH*(*)	inp	Output Asci File
C
C Audit trail:
C	New
C				M.A.Holdaway	Oct 16 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	PDT, FILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDPUT')
C
      CHARACTER*(SYSMXNAM)	FILESYS
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL FILSYSEX (FILE, FILESYS)
      IF (FILESYS .EQ. 'SDE') THEN
         CALL DATWRITE (PDT, FILE)
      ELSE
         CALL VSDAPUT (PDT, FILE)
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



