C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)closure.f	1.6    11/15/94
C
      SUBROUTINE SDEMAIN
C
CD Program to work with closure phase information
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	New version which can do triple-product processing as well
C				T.J.Cornwell	March 27 1989
C	Changed to VISPUT to get FITS output
C				T.J.Cornwell	November 1 1990
C	Added multiple field models.  Things will break if there is
C	emission in regions of overlap!
C				D.S.Briggs	June 6 1994
C	Added inputs to history of output file.
C				D.S.Briggs	July 11 1994
C	Added Editing
C    				T.J. Cornwell	November 14 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CLOSURE')
C
      INTEGER MAXMOD
      PARAMETER (MAXMOD=10)
C
      CHARACTER*(SYSMXNAM)	MODFILE(MAXMOD)
      DATA MODFILE/MAXMOD*' '/
C
      CHARACTER*(SYSMXNAM)	VISFILE, NVISFILE,
     1			MODE, METHOD
      REAL		TINT, THRES
      INTEGER		NDUMMY, I
C==================================================================
      CALL MSGWELCO ('I use closure phase information')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Tavg', TINT, 1, NDUMMY)
      IF (TINT.LE.0.0) TINT = 10.0
      CALL USRGETR ('Threshold', THRES, 1, NDUMMY)
      CALL USRGETC ('SolType', MODE, 1, NDUMMY)
      CALL USRGETC ('Method', METHOD, 1, NDUMMY)
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 10, NDUMMY)
      CALL USRGETC('NewVis', NVISFILE, 1, NDUMMY)
      CALL VISGET ('Vis', VISFILE, 'I,V', '*', ' ')
C
      CALL DATPUTR ('Vis', 'TINT', TINT, 1)
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
C
C Now calculate model visibility
C
      CALL FILIMGGE ('Model', MODFILE(1), ' ')
      CALL IMGTOVIS ('Vis', 'MOD/I', 'Model', 'Modvis', 'DModel')
      IF (ERROR) GO TO 999
C
      DO 100, I = 2, MAXMOD
         IF (MODFILE(I).NE.' ') THEN
            CALL FILIMGGE ('OffModel', MODFILE(I), ' ')
            CALL VISCLONE ('Vis', 'OBS', 'I', 'TMP')
            CALL IMGTOVIS ('Vis', 'TMP/I', 'OffModel',
     $         'OffModvis', 'OffDModel')
            CALL ARRLC ('Vis/OBS/I/VIS', 1.0, 'Vis/TMP/I/VIS', 1.0,
     $         'Vis/OBS/I/VIS')
            CALL DATDELET ('Vis/TMP/I/VIS')
            CALL DATDELET ('OffModel')
            CALL DATDELET ('OffModvis')
            CALL DATDELET ('OffDModel')
         END IF
 100  CONTINUE
C
C Do solution
C
      IF (METHOD.EQ.'Triple') THEN
         CALL MSGPUT ('Using triple-product approach', 'I')
         CALL MSGPUT ('---Be prepared to wait a long time', 'I')
         IF (MODE.EQ.'AMPPHI') THEN
            CALL MSGPUT ('Estimating both amplitude and phase', 'I')
         ELSE
            CALL MSGPUT ('Estimating phase only', 'I')
         END IF
         CALL VISTRP ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', MODE)
      ELSE
         CALL MSGPUT ('Using self-calibration approach', 'I')
         IF (MODE.EQ.'AMPPHI') THEN
            CALL MSGPUT ('Correcting both amplitude and phase', 'I')
         ELSEIF (MODE.EQ.'AMPNORMPHI') THEN
            CALL MSGPUT ('Correcting renormalized amplitude and phase',
     $           'I')
         ELSE
            CALL MSGPUT ('Correcting phase only', 'I')
         END IF
         CALL VISSCAL ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', MODE)
      END IF
C
C Edit data?
C
      IF (THRES.GT.0.0) THEN
         CALL MSGPUT ('Editing data', 'I')
         CALL IMGTOVIS ('Vis', 'MOD/I', 'Model', 'Modvis', 'DModel')
         CALL VISEDIT ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', THRES, ' ')
      END IF
      IF (ERROR) GO TO 999
C
C The bare minimum reasonable history
C
      CALL HISINPUT ('Vis')
C
C Write answers
C
      IF (NVISFILE.NE.' ') THEN
         CALL VISPUT ('Vis', NVISFILE, 'OBS', 'I,V', '*', ' ')
      END IF
C
 999  CONTINUE
      END
