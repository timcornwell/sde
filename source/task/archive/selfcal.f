C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to selfcal
C
CS Arguments: CALL SDEMAIN
CA Audit trail:
CA	Changed to VISPUT
CA				T.J.Cornwell	Feb 3 1989
CA	Changed to IMGTOVIS
CA				T.J. Cornwell	Feb 18 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SELFCAL')
C
      CHARACTER*(SYSMXNAM)	VISFILE, MODFILE, NVISFILE,
     1			CFTYPE, MODE, GLFILE, PLFILE
      REAL		TINT
      INTEGER		NDUMMY
C==================================================================
      CALL MSGWELCO ('I self-calibrate')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Tavg', TINT, 1, NDUMMY)
      CALL USRGETC('SolType', MODE, 1, NDUMMY)
      IF (TINT.LE.0.0) TINT = 10.0
      CALL USRGETC ('ConvType', CFTYPE, 1, NDUMMY)
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC('NewVis', NVISFILE, 1, NDUMMY)
      CALL USRGETC('Outfile', GLFILE, 1, NDUMMY)
      CALL USRGETC('Plotfile', PLFILE, 1, NDUMMY)
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
C
      CALL DATPUTR ('Vis', 'TINT', TINT, 1)
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
      CALL FILIMGGE ('Model', MODFILE, ' ')
C
C Now calculate model visibility
C
      CALL DATPUTC ('Model', 'CFTYPE', CFTYPE, 1)
      CALL IMGTOVIS ('Vis', 'MOD/I', 'Model', 'Modvis', 'DModel')
C
C Do selfcal
C
      IF (MODE.EQ.'AMPPHI') THEN
         CALL MSGPUT ('Correcting both amplitude and phase', 'I')
      ELSE
         CALL MSGPUT ('Correcting phase only', 'I')
      END IF
      CALL VISSCAL ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', MODE)
      IF (ERROR) GO TO 999
C
      IF (GLFILE.NE.' ') THEN
         CALL TXTOPEN ('Gains', GLFILE, 'NEW')
         CALL CALLIST ('Gains', 'Vis', 'OBS/I')
         CALL TXTCLOSE ('Gains')
      END IF
C
C Plot gains?
C
      IF (PLFILE.NE.' ') THEN
         CALL VISANTPO ('Vis')
         CALL CALPLOT (PLFILE, 'Vis', 'OBS/I')
      END IF
C
      IF (NVISFILE.NE.' ') THEN
         CALL VISPUT ('Vis', NVISFILE, 'OBS/I', '*', '*')
      END IF
C
 999  CONTINUE
      END
