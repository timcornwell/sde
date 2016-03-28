C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdetect.f	1.1 8/20/92
C
      SUBROUTINE SDEMAIN
C
CD Program to detect a signal in visibility data
C
C Audit trail:
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISEDIT')
C
      CHARACTER*(SYSMXNAM)	VISFILE, MODFILE
      REAL		SUMWT, CHISQ, AMP
      INTEGER		NDUMMY
      LOGICAL		SUBMOD, FILEXIST
C==================================================================
      CALL MSGWELCO ('I assess fit of a model to visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 1, NDUMMY)
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
      SUBMOD=FILEXIST(MODFILE)
C
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
      IF(SUBMOD) THEN
         CALL FILIMGGE ('Model', MODFILE, ' ')
         CALL IMGTOVIS ('Vis', 'MOD/I', 'Model', 'Modvis', 'DModel')
      ELSE
         CALL ARRSETCO ('Vis/MOD/I/VIS', 0.0, 0.0)
      ENDIF
C
      CALL VISDET ('Vis', 'OBS/I', 'MOD/I', AMP, SUMWT, CHISQ)
      IF (ERROR) GO TO 999
C
      WRITE (MESSAGE, 1100) AMP
 1100 FORMAT ('Amplitude of model in data = ',1PE12.4)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1200) SUMWT, CHISQ
 1200 FORMAT ('Sum of weights = ',1PE12.4,', Chisq = ',1PE12.4)
      CALL MSGPUT (MESSAGE, 'I')
      IF(SUMWT.GT.0.0) THEN
         WRITE(MESSAGE, 1300) SQRT(CHISQ/SUMWT)
 1300    FORMAT ('RMS error per weight = ',1PE12.4)
         CALL MSGPUT (MESSAGE, 'I')
      ENDIF
C
 999  CONTINUE
      END

