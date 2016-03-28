C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ddiff.f	1.1	 5/4/93
C
      SUBROUTINE SDEMAIN
C
CD Program to form the difference of two D term solutions
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	April 27 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'DDIFF')
C
      CHARACTER*(SYSMXNAM) 	DFILE1, DFILE2, OUTDFILE
      INTEGER			NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I rotate D terms')
C
      CALL USRCTL
      CALL USRGETC ('D1', DFILE1, 1, NDUMMY)
      CALL USRGETC ('D2', DFILE2, 1, NDUMMY)
      CALL USRGETC ('Ddiff', OUTDFILE, 1, NDUMMY)
C
      CALL VSDGET ('PDT1', DFILE1)
      CALL VSDGET ('PDT2', DFILE2)
C 'PDT3' is a dummy to hold the difference PDT
      CALL VSDGET ('PDT3', DFILE1)
C
      CALL VSDDIFF ('PDT1', 'PDT2', 'PDT3')
      IF (ERROR) GOTO 999
C
      IF (OUTDFILE .NE. ' ') THEN
         CALL VSDPUT ('PDT3', OUTDFILE)
      ENDIF
C
      IF (ERROR) GOTO 999
C
 999  CONTINUE
      END
