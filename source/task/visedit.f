C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visedit.f	1.6    5/17/91
C
      SUBROUTINE SDEMAIN
C
CD Program to edit visibility data
C
C Audit trail:
C	Fixed VISPUT
C				T.J.Cornwell    Nov 1 1990
C	If no model is specified then it simply flags on high values
C				T.J.Cornwell	March 11 1991
C	Changed to use FILEXIST
C				T.J.Cornwell	May 8 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISEDIT')
C
      CHARACTER*(SYSMXNAM)	VISFILE, MODFILE, NVISFILE
      REAL		TINT, THRES
      INTEGER		NDUMMY
      LOGICAL		SUBMOD, FILEXIST
C==================================================================
      CALL MSGWELCO ('I edit visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Threshold', THRES, 1, NDUMMY)
      CALL USRGETR ('Tavg', TINT, 1, NDUMMY)
      IF (TINT.LE.0.0) TINT = 10.0
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC('NewVis', NVISFILE, 1, NDUMMY)
      CALL VISGET ('Vis', VISFILE, '*', '*', ' ')
      SUBMOD=FILEXIST(MODFILE)
C
      CALL DATPUTR ('Vis', 'TINT', TINT, 1)
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
      IF(SUBMOD) THEN
         CALL FILIMGGE ('Model', MODFILE, ' ')
         CALL IMGTOVIS ('Vis', 'MOD/I', 'Model', 'Modvis', 'DModel')
      ELSE
         CALL ARRSETCO ('Vis/MOD/I/VIS', 0.0, 0.0)
      ENDIF
C
      CALL VISEDIT ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', THRES, ' ')
      IF (ERROR) GO TO 999
C
      IF (NVISFILE.NE.' ') THEN
         CALL VISPUT ('Vis', NVISFILE, 'OBS', 'I,Q,U,V', '*', ' ')
      END IF
C
 999  CONTINUE
      END
