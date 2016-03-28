C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trp.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to work with triple products
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRP')
C
      CHARACTER*(SYSMXNAM)	VISFILE, MODFILE, NVISFILE,
     1			CFTYPE, MODE
      REAL		TINT
      INTEGER		NDUMMY
C==================================================================
      CALL MSGWELCO ('I triple-produce')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Tavg', TINT, 1, NDUMMY)
      IF (TINT.LE.0.0) TINT = 10.0
      CALL USRGETC ('ConvType', CFTYPE, 1, NDUMMY)
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC('NewVis', NVISFILE, 1, NDUMMY)
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
C
      CALL DATPUTR ('Vis', 'TINT', TINT, 1)
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
      CALL FILIMGGE ('IModel', MODFILE, ' ')
      CALL IMGDOUBL ('IModel', 'Model')
      CALL DATDELET ('IModel')
C
C Now calculate model visibility
C
      CALL IMGTOVIS ('Vis', 'MOD/I', 'Model', 'Modvis', 'Model')
C
C Do BS
C
      CALL VISTRP ('Vis', 'OBS/I', 'MOD/I', 'OBS/I')
      IF (ERROR) GO TO 999
C
      IF (NVISFILE.NE.' ') THEN
         CALL MSGPUT ('Writing new visibility file as SDE file', 'I')
         CALL DATWRITE ('Vis', NVISFILE)
      END IF
C
 999  CONTINUE
      END
