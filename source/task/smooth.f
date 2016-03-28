C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)smooth.f	1.7    7/16/94
C
      SUBROUTINE SDEMAIN
C
CD Program to smooth images with an elliptical Gaussian 
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Turned on flag to allow for exact FFTs
C				D.S.Briggs	30 Mar 1992
C       Added BEAMFILE
C				D.S.Briggs	1 May 1992
C	Added smoothing for images in uv plane
C				D.S.Briggs	23 Jan 1993
C	Tweaked beamfit selection
C				D.S.Briggs	June 14 1994
C	Stash beam parameters in output header
C				D.S.Briggs	July 16 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SMOOTH')
C
      INTEGER 		NAX, NAXIS(SYSMXDIM), NDUMMY
      REAL		BEAM(4)
      CHARACTER*(SYSMXNAM)	INFILE, SMFILE, NORM, BEAMFILE, FITALG
      CHARACTER*8	IMTYPE
      CHARACTER*1	ATYPE
C==================================================================
      CALL MSGWELCO ('I smooth images with an elliptical Gaussian')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Smooth', SMFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('BeamFile', BEAMFILE, 1, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETC ('Normalize', NORM, 1, NDUMMY)
C
C Get input Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL DATGETAR ('Image', NAX, NAXIS, ATYPE, NDUMMY)
      CALL IMGTYPE ('Image', IMTYPE)
C
C Enforce a restriction
C
      IF (IMTYPE.EQ.'VIS'.AND.(NORM(1:1).EQ.'P')) THEN
         CALL MSGPUT ('Sorry Peak normalization NYI for vis data','E')
         GO TO 999
      END IF
C
      IF ((IMTYPE.EQ.'VIS').AND.(ATYPE.NE.'X'))
     $     CALL ARRCVTX ('Image', 'Image')
      CALL IMGCLONE ('Image', 'Smooth')
C
C Dig out beam parameters
C
      CALL FILBEMGE (BEAMFILE, BEAM, FITALG, 'TAPER')
C
C Smooth
C
      CALL DATPUTC ('Image', 'FFTSIZE', 'EXACT', 1)
      CALL IMGSMOOT ('Image', BEAM, 'Smooth', 'Swork')
      IF (NORM(1:1) .NE. 'P') THEN
         CALL MSGPUT ('Smoothing beam has integral of 1.0', 'I')
      ELSE
         CALL MSGPUT ('Smoothing beam has peak of 1.0', 'I')
         CALL IMGP2PB ('Smooth', BEAM, 'Smooth')
      ENDIF
C
C History and header tweaking.
C
      CALL HISINPUT ('Smooth')
      CALL DATPUTR ('Smooth', 'BMAJ', BEAM(1)/3600.0, 1)
      CALL DATPUTR ('Smooth', 'BMIN', BEAM(2)/3600.0, 1)
      CALL DATPUTR ('Smooth', 'BPA',  BEAM(3), 1)
      CALL DATPUTR ('Smooth', 'BZ', BEAM(4)/3600.0, 1)
C
C Write out answer
C
      IF ((IMTYPE.EQ.'VIS').AND.(ATYPE.EQ.'R'))
     $     CALL ARRCVTR ('Smooth', 'Smooth')
      CALL FILIMGXP ('Smooth', SMFILE, 'QU', ' ')
C
 999  CONTINUE
      END
