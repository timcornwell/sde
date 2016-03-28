C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gainapply.f	1.2    9/15/94
C
      SUBROUTINE SDEMAIN
C
CD Program to apply gains to visibility data.
C
C Audit trail:
C       Original version
C				D.S.Briggs	12 Sept 1991
C	Moved  GAITYPE into a header item to be consistent with GAIAPPLY,
C	added inputs for TAVG and AVGMODE.
C				M.A. Holdaway	15 Sept 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GAINAPPLY')
C
      CHARACTER*(SYSMXNAM)	VISFILE, CALFILE, NVISFILE, STOKES,
     $   			INTERP, BUFF, OBSCLASS, AVGMODE
      REAL			TAVG
      INTEGER		NDUMMY
C
      CHARACTER*(SYSMXNAM)	STRM2
C==================================================================
      CALL MSGWELCO ('I apply gains to visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Cal', CALFILE, 1, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETC ('InterpMode', INTERP, 1, NDUMMY)
      CALL USRGETR ('SmoothTime', TAVG, 1, NDUMMY)
      CALL USRGETC ('SmoothMode', AVGMODE, 1, NDUMMY)
      CALL USRGETC ('NewVis', NVISFILE, 1, NDUMMY)
C
      TAVG = TAVG / 86400.0
      IF (TAVG .LT. 0.0) TAVG = 0.0
      IF (INTERP .NE. '2PT') INTERP = 'BOXCAR'
      IF (AVGMODE .NE. 'AMPPHI') AVGMODE = ' '
C
      IF (CALFILE.EQ.' ') CALFILE = VISFILE
      IF (STOKES.EQ.' ') STOKES = 'I'
      BUFF = INTERP
      CALL STRUC (BUFF, INTERP)
      IF (INTERP.EQ.' ') INTERP = 'BOXCAR'
C
      CALL VISGET ('Vis', VISFILE, STOKES, '*', ' ')
      CALL VISGET ('Cal', CALFILE, STOKES, '*', ' ')
C
      OBSCLASS = STRM2('OBS',STOKES(1:1))
      CALL DATPUTC (STRM2('Vis', OBSCLASS), 'GAITYPE', INTERP, 1)
      CALL DATPUTR (STRM2('Vis', OBSCLASS), 'GAITINT', TAVG, 1)
      CALL DATPUTC (STRM2('Vis', OBSCLASS), 'GAIAVG', AVGMODE, 1)
C
      CALL GAIAPPLY ('Vis', OBSCLASS, 'Cal', OBSCLASS, 'Vis', 
     $     OBSCLASS)
C
      CALL VISPUT ('Vis', NVISFILE, 'OBS', STOKES, '*', ' ')
C
 999  CONTINUE
      END
