C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)img2mod.f	1.1    7/15/93
C
      SUBROUTINE SDEMAIN
C
CD Writes portions of an image to SDE model form
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	13 July 1993
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMG2MOD')
C
      CHARACTER*(SYSMXNAM)	IMAGE, MODEL, MASK
C
      INTEGER		NDUMMY
C=======================================================================
      CALL MSGWELCO ('I write pixels to SDE models')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
      CALL USRGETC ('Model', MODEL, 1, NDUMMY)
      CALL USRGETC ('Box', MASK, 1, NDUMMY)
C
      CALL FILIMGGE ('Image', IMAGE, ' ')
      CALL FILMASGE ('Mask', MASK, 'Image')
C
      CALL IMGIM2MO ('Image', 'Mask', 'Model')
C
      CALL FILDEL(MODEL)
      CALL FILPUTMO ('Model', MODEL)
C
C Can jump to here if an error found
C
 999  CONTINUE
      END
