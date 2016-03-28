C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsmooth.f	1.1    11/1/94

      SUBROUTINE SDEMAIN
C
CD Program to smooth images with an elliptical Gaussian 
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Nov 1 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSMOOTH')
C
      INTEGER 		NDUMMY
      REAL		BEAM(4)
      CHARACTER*(SYSMXNAM)	INFILE, SMFILE, BEAMFILE, FITALG
      CHARACTER*(SYSMXNAM)	SUBCLASS, STOKES
C==================================================================
      CALL MSGWELCO (
     $   'I taper a visibility db with an elliptical Gaussian')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Vis', INFILE, 1, NDUMMY)
      CALL USRGETC ('Smooth', SMFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('BeamFile', BEAMFILE, 1, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETC ('Stokes', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, STOKES)
C
      SUBCLASS = 'OBS/' // STOKES(1:1)
      CALL FILBEMGE (BEAMFILE, BEAM, FITALG, 'TAPER')
C
      CALL VISGET ('Vis', INFILE, STOKES(1:1), '*', ' ')
C
      CALL VISTAPER ('Vis', SUBCLASS, BEAM, SUBCLASS)
C
      CALL HISINPUT ('Vis')
      CALL VISPUT ('Vis', SMFILE, 'OBS', STOKES(1:1), '*', ' ')
C
 999  CONTINUE
      END
