C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visplot.f	1.4	 12/22/92
C
      SUBROUTINE SDEMAIN
C
CD Program to plot visibility data
C
C Audit trail:
C	New task
C				M.A.Holdaway	Oct 23 1991
C	Made it into a loop
C				M.A.Holdaway	?
C	Added RL, LR;  Added REAL vs IMAG plot;  Added STOKES in plot label
C				M.A.Holdaway	Dec 22 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPLOT')
C
      CHARACTER*(SYSMXNAM)	VISFILE, DEVAM, DEVPH, DEVRI, 
     $   			STOKES, SUBCLASS, OLDFILE
      REAL			TIME(2), OLDTIME(2), DT
      INTEGER			TIMR(8), ANT1, ANT2
      LOGICAL		DATEXIST
      INTEGER		NDUMMY, NSEL, STYLE
      REAL		UVLIMITS(2)
      COMPLEX		ONEX, JX, MJX
C==================================================================
      CALL MSGWELCO ('I list visibility data')
      OLDFILE = ' '
      OLDTIME(1) = -999.
      OLDTIME(2) = -999.
 1    CONTINUE
C
C Get input parameters
C
      CALL USRCTL
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETR ('UVLimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETC ('DevAM', DEVAM, 1, NDUMMY)
      CALL USRGETC ('DevPH', DEVPH, 1, NDUMMY)
      CALL USRGETC ('DevRI', DEVRI, 1, NDUMMY)
      CALL USRGETI ('Ant1', ANT1, 1, NDUMMY)
      CALL USRGETI ('Ant2', ANT2, 1, NDUMMY)
      CALL USRGETI ('Style', STYLE, 1, NDUMMY)
C
      SUBCLASS = 'OBS/'//STOKES
      IF (VISFILE .NE. OLDFILE) THEN
         OLDFILE = VISFILE
         IF (DATEXIST ('Vis')) CALL DATDELET ('Vis')
         CALL VISGET ('Vis', VISFILE, 'IQUV', '*', ' ')
         CALL VISSCLON ('Vis/OBS', 'Q', 'RL')
         CALL VISSCLON ('Vis/OBS', 'Q', 'LR')
         ONEX = CMPLX(1.0, 0.0)
         JX   = CMPLX(0.0, 1.0)
         MJX  = CMPLX(0.0, -1.0)
         CALL ARRXLC ('Vis/OBS/Q/VIS', ONEX, 'Vis/OBS/U/VIS', 
     $      JX, 'Vis/OBS/RL/VIS')
         CALL ARRXLC ('Vis/OBS/Q/VIS', ONEX, 'Vis/OBS/U/VIS',
     $      MJX, 'Vis/OBS/LR/VIS')
      ENDIF
      IF(DATEXIST('Vis/TIME')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         IF (TIME(1) .NE. OLDTIME(1) .OR. TIME(2) .NE. OLDTIME(2)) THEN
            OLDTIME(1) = TIME(1)
            OLDTIME(2) = TIME(2)
            CALL VISSEL ('Vis', SUBCLASS, TIME, UVLIMITS, NSEL)
            IF (ERROR) GO TO 999
            WRITE (MESSAGE, 1000) NSEL
 1000       FORMAT ('Selected ',I7,' visibilities')
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
      ELSE
         CALL MSGPUT ('No TIME information', 'W')
         CALL ERRCANCE
         GOTO 1
      END IF
C
      IF (DEVPH .NE. ' ' .OR. DEVAM .NE. ' ') THEN
         CALL VISTPL ('Vis', SUBCLASS, DEVPH, DEVAM, ANT1, ANT2, DT, 
     $      STOKES, STYLE)
      ENDIF
      IF (DEVRI .NE. ' ') THEN
         CALL VISRIPLT ('Vis', SUBCLASS, DEVRI, ANT1, ANT2, STOKES, 
     $      STYLE)
      ENDIF
      GO TO 1
 900  CONTINUE
C
 999  CONTINUE
      END
