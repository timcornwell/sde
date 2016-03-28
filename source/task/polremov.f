C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)polremov.f	1.1	 7/21/94
C
      SUBROUTINE SDEMAIN
C
CD Program to remove polarized part of emission from total intensity,
C  given a polarization position angle
C
C Audit trail:
C	New Program
C				M.A. Holdaway	July 21 1994
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'POLREMOV')
C
      CHARACTER*(SYSMXNAM)	VISFILE, CHIFILE,
     $   			OUTFILE
      INTEGER		NDUMMY
C
C==================================================================
      CALL MSGWELCO ('I solve for D terms')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
      CALL USRGETC ('ChiFile', CHIFILE, 1, NDUMMY)
C
      CALL VISGET ('Vis', VISFILE, '*', '*', ' ')
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
      CALL VISCLONE ('Vis', 'OBS', 'Q', 'MOD')
      CALL VISCLONE ('Vis', 'OBS', 'U', 'MOD')
      CALL VISCLONE ('Vis', 'OBS', 'V', 'MOD')
C
      CALL FILGETN ('Chi', CHIFILE)
      CALL VISPOLR ('Vis', 'Chi')
C
      IF (OUTFILE.NE.' ') THEN
         CALL VISPUT ('Vis', OUTFILE, 'OBS', 'IQUV', '*', ' ')
      END IF
C
 999  CONTINUE
      END

