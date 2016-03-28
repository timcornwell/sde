C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visflagv.f	1.4    7/17/92
C
      SUBROUTINE SDEMAIN
C
CD Program to edit visibility data by flagging the V axis
C
C Audit trail:
C	New task
C				T.J. Cornwell     Sept 1 1989
C	Fixed VISPUT
C				T.J.Cornwell    Nov 1 1990
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISFLAGV')
C
      CHARACTER*(SYSMXNAM)	VISFILE, MODFILE, NVISFILE,
     1			CFTYPE, MODE
      REAL		TINT, THRES
      INTEGER		NDUMMY
C==================================================================
      CALL MSGWELCO ('I edit visibility data by flagging the v axis')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Threshold', THRES, 1, NDUMMY)
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('NewVis', NVISFILE, 1, NDUMMY)
      CALL VISGET ('Vis', VISFILE, '*', '*', ' ')
C
      CALL VISFLAGV ('Vis', 'OBS/I', ' ', THRES)
C
      IF (NVISFILE.NE.' ') THEN
         CALL HISINPUT ('Vis')
         CALL VISPUT ('Vis', NVISFILE, 'OBS', 'I,Q,U,V', '*', ' ')
      END IF
C
 999  CONTINUE
      END
