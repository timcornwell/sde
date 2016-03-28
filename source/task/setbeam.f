C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)setbeam.f	1.1    11/7/94
C
      SUBROUTINE SDEMAIN
C
CD Set Gaussian Beam parameters in image header
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Sept 13 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SETBEAM')
C
      INTEGER 		NDUMMY
      CHARACTER*(SYSMXNAM)	INFILE, OUTFILE, BEAMFILE, FITMODE,
     $   		BUNIT
      REAL		BEAM(4)
C==================================================================
      CALL MSGWELCO ('I set beam parameters in image headers')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('BFile', BEAMFILE, 1, NDUMMY)
      CALL USRGETC ('BUnit', BUNIT, 1, NDUMMY)
      CALL USRGETC ('FitMode', FITMODE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
C
C Get Image and BEAM
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL FILBEMGE (BEAMFILE, BEAM, FITMODE, 'BEAM')
C
C Stash it
C
      CALL DATPUTR ('Image', 'BMAJ', BEAM(1)/3600.0, 1)
      CALL DATPUTR ('Image', 'BMIN', BEAM(2)/3600.0, 1)
      CALL DATPUTR ('Image', 'BPA',  BEAM(3), 1)
      CALL DATPUTR ('Image', 'BZ', BEAM(4)/3600.0, 1)
      IF (BUNIT.NE.' ')
     $   CALL DATPUTC ('Image', 'BUNIT', BUNIT, 1)
C
C History
C
      CALL HISINPUT ('Image')
C
C Write output
C
      CALL DATRENAM ('Image', 'Output')
      CALL FILIMGPU ('Output', OUTFILE, ' ')
C
 999  CONTINUE
      END
