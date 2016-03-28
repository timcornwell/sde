C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)drotate.f	1.1	 5/4/93
C
      SUBROUTINE SDEMAIN
C
CD Program to rotate D terms
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	April 14 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'DROTATE')
C
      CHARACTER*(SYSMXNAM) 	INDFILE, OUTDFILE
      REAL			ROTATE
      INTEGER			NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I rotate D terms')
C
      CALL USRCTL
      CALL USRGETC ('Din', INDFILE, 1, NDUMMY)
      CALL USRGETC ('Dout', OUTDFILE, 1, NDUMMY)
      CALL USRGETR ('Rotate', ROTATE, 1, NDUMMY)
C
      CALL VSDGET ('PDT', INDFILE)
      CALL VSDROTAT ('PDT', ROTATE)
      CALL VSDPUT ('PDT', OUTDFILE)
C
      IF (ERROR) GOTO 999
C
 999  CONTINUE
      END
