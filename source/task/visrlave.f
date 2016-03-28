C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrlave.f	1.2	 6/21/93
C
      SUBROUTINE SDEMAIN
C
CD Program to apply polarization D terms to a VIS file
C
C Audit trail:
C	New Program
C				M.A. Holdaway	April 22 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRLAVE')
C
      CHARACTER*(SYSMXNAM)	VISFILE, TXTFILE
      LOGICAL		DIVI
      REAL		TIME(2), UVLIMITS(2)
      INTEGER		NSEL, TIMR(8), ROTATE
      INTEGER		NDUMMY
C
      REAL		SLON, SLAT
C==================================================================
      CALL MSGWELCO ('I get ave (RL+LR*)/2 vs TIME')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('2Timerange', TIME, 2, NDUMMY)
      CALL USRGETI ('8Timerange', TIMR, 8, NDUMMY)
      CALL USRGETI ('Rotate', ROTATE, 1, NDUMMY)
      CALL USRGETL ('DivideI', DIVI, 1, NDUMMY)
      CALL USRGETC ('Out', TXTFILE, 1, NDUMMY)
C
      CALL VISGET ('Vis', VISFILE, '*', '*', ' ')
      IF (TIME(2) .EQ. 0.0) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
      ENDIF
      CALL VISSEL ('Vis', 'OBS/I', TIME, UVLIMITS, NSEL)
      CALL VISSEL ('Vis', 'OBS/Q', TIME, UVLIMITS, NSEL)
      CALL VISSEL ('Vis', 'OBS/U', TIME, UVLIMITS, NSEL)
C
C VLA site hardwired in
C
      SLON = 107.6177275
      SLAT = 34.078749167
      CALL DATPUTR ('Vis', 'SLON', SLON, 1)
      CALL DATPUTR ('Vis', 'SLAT', SLAT, 1)
C
      CALL VISPAVE ('Vis', ROTATE, TXTFILE, DIVI)
C
 999  CONTINUE
      END

