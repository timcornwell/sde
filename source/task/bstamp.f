C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)bstamp.f	1.1    2/21/95
C
      SUBROUTINE SDEMAIN
C
CD Program to transfer beam parameters
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Feb 21 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'BSTAMP')
C
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE, BEAMFILE, FITALG
      REAL		BEAM(4)
      INTEGER		NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I copy images')
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('BeamFile', BEAMFILE, 1, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
C
C Get Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
C
C Get Beam
C
      CALL FILBEMGE (BEAMFILE, BEAM, FITALG, 'TAPER')
C
C Stash it
C
      CALL DATRENAM ('Image','Output')
      CALL DATPUTR ('Output', 'BMAJ', BEAM(1)/3600.0, 1)
      CALL DATPUTR ('Output', 'BMIN', BEAM(2)/3600.0, 1)
      CALL DATPUTR ('Output', 'BPA',  BEAM(3), 1)
      CALL DATPUTR ('Output', 'BZ', BEAM(4)/3600.0, 1)
C
C Write result 
C
      CALL HISINPUT ('Output')
      CALL FILIMGPU ('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
