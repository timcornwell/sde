C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vismodel.f	1.4    11/22/91
C
      SUBROUTINE SDEMAIN
C
CD Program to insert a model into a vis dataset. The model is read from an
C external ASCII file. The image header is fabricated from a uv data
C file.
C
C Audit trail:
C	Changed to VISPUT
C				T.J.Cornwell	Feb 3 1989
C	Fixed VISPUT
C				T.J.Cornwell    Nov 1 1990
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISMODEL')
C
      REAL		CELLSIZE(3), IMSIZE(3), SHIFT(3)
      LOGICAL		DFT
      INTEGER 		NDUMMY, I
      CHARACTER*(SYSMXNAM)	MODFILE, VISFILE, IMGFILE, NVISFILE
      DATA		SHIFT	/3*0.0/
C==================================================================
C
      CALL MSGWELCO ('I insert a model into a visibility data set')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETL ('DFT', DFT, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('NVis', NVISFILE, 1, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
C
C Now get uv data
C
      CALL VISGET ('Vis', VISFILE, '*', '*', ' ')
C
C Now get model
C
      IF (IMGFILE.NE.' ') THEN
         CALL FILIMGGE ('Image', IMGFILE, ' ')
         CALL MSGPUT ('Coordinates of model image:', 'I')
         CALL CRDLIST ('Image')
      ELSE IF (MODFILE.NE.' ') THEN
         CALL IMGMAKE ('Vis/OBS/I', CELLSIZE, IMSIZE, SHIFT,
     1      'R', 'Image')
         CALL FILGETMO ('Model', MODFILE)
         CALL MSGPUT ('Listing of model', 'I')
         CALL MODLIST ('Model')
         CALL MODIMG ('Model', 'Image')
         CALL MSGPUT ('Coordinates of model image:', 'I')
         CALL CRDLIST ('Image')
      END IF
C
C Do direct Fourier transform
C
      CALL MSGPUT ('Transforming model', 'I')
      IF (.NOT.DFT) THEN
         CALL IMGGRIDC ('Image', 'Image', 'CORRECT')
         CALL IMGFFT ('Image', 'Modvis')
         CALL VISDEGRI ('Vis', 'OBS/I', 'Modvis')
      ELSE
         CALL IMGDFT ('Image', 'Vis', 'OBS/I')
      END IF
C
      CALL VISPUT ('Vis', NVISFILE, 'OBS', 'I,Q,U,V', '*', ' ')  
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
