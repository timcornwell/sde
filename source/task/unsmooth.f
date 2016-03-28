C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)unsmooth.f	1.3	 6/14/94
C
      SUBROUTINE SDEMAIN
C
CD Program to unsmooth images or models with an elliptical Gaussian 
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	16 July 1992
C	Limited ability to unsmooth analytic models added
C				D.S.Briggs	Feb 1993
C	Tweaked beamfit selection
C				D.S.Briggs	June 14 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UNSMOOTH')
C
      INTEGER 		NDUMMY
      REAL		BEAM(4)
      CHARACTER*(SYSMXNAM)	INFILE, OIMGFILE, BEAMFILE, XOIMGFIL,
     $   		FILSYS, MODFILE, OMODFILE, FITALG
C==================================================================
      CALL MSGWELCO (
     $   'I unsmooth images or models with an elliptical Gaussian')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('OutImage', OIMGFILE, 1, NDUMMY)
      CALL USRGETC ('XOutImage', XOIMGFIL, 1, NDUMMY)
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC ('OutModel', OMODFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('BeamFile', BEAMFILE, 1, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
C
C Dig out beam parameters
C
      CALL FILBEMGE (BEAMFILE, BEAM, FITALG, 'TAPER')
C
C Process the appropriate stuff
C
      IF (INFILE.NE.' ') THEN
         CALL FILIMGGE ('Image', INFILE, ' ')
         CALL DATPUTC ('Image', 'FFTSIZE', 'EXACT', 1)
         CALL IMGUNSMO ('Image', BEAM, 'Unsmooth', 'XImage')
      END IF
C
      IF (MODFILE.NE.' ') THEN
         CALL FILGETMO ('Model', MODFILE)
         CALL MODLIST ('Model')
         CALL MODDCONV ('Model', BEAM, 'DeconvolvedModel')
         CALL MODLIST ('DeconvolvedModel')
      END IF
C
C Write out answer
C
      IF (OIMGFILE.NE.' ') CALL FILIMGPU ('Unsmooth', OIMGFILE)
      IF (OMODFILE.NE.' ') CALL FILPUTMO ('Model', OMODFILE)
C
C Yeah, this is a little special purpose, but it's handy.  If we're
C  restricted to real images on output, take the ABS of the complex
C  unsmoothed uv plane image.
C
      IF (XOIMGFIL.NE.' ') THEN
         CALL FILSYSEX (XOIMGFIL, FILSYS)
         IF (FILSYS.NE.'SDE') THEN
            CALL ARRABS ('XImage', 'RXImage')
            CALL HEDCOPY ('XImage', 'RXImage')
            CALL HISCOPY ('XImagr', 'RXImage')
            CALL FILIMGPU ('RXImage', XOIMGFIL, ' ')
         ELSE
            CALL FILIMGPU ('XImage', XOIMGFIL, ' ')
         END IF
      END IF
C
 999  CONTINUE
      END


