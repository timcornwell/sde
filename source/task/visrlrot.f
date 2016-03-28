C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrlrot.f	1.1	 5/4/93
C
      SUBROUTINE SDEMAIN
C
CD Program to rotate the RL phase of the visibilities
C
C Audit trail:
C	New Program
C				M.A. Holdaway	April 28 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRLROT')
C
      CHARACTER*(SYSMXNAM)	VISFILE, OUTFILE
      REAL		THETA
      INTEGER		NDUMMY
C
C==================================================================
      CALL MSGWELCO ('I rotate the RL visibility phase')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
      CALL USRGETR ('Theta', THETA, 1, NDUMMY)
C
      CALL VISGET ('Vis', VISFILE, '*', '*', ' ')
      CALL VISRLROT ('Vis', THETA)
      IF (OUTFILE.NE.' ') THEN
         CALL VISPUT ('Vis', OUTFILE, 'OBS', 'IQUV', '*', ' ')
      END IF
C
 999  CONTINUE
      END

