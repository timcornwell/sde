C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ldecs.f	1.3    12/15/94
C
      SUBROUTINE SDEMAIN
C
CD Program to do linear deconvolutions plus
C subsequent smoothing with an elliptical Gaussian 
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Add conversion to Jy/Beam in the final image
C				M.A. Holdaway	Dec 15, 1994
C--------------------------------------------------------------------
#include 		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'LDECS')
C
      INTEGER 		NDUMMY
      REAL		BEAM(4), MIND, DATFGETR
      CHARACTER*(SYSMXNAM)	INFILE, BMFILE, LDSFILE
C
C==================================================================
C
      CALL MSGWELCO ('I do linear deconvolution and smoothing')
      CALL USRCTL
C
C                         Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Deconv', BMFILE, 1, NDUMMY)
      CALL USRGETC ('LDS', LDSFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 3, NDUMMY)
      CALL USRGETR ('MinD', MIND, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Get input Images
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL FILIMGGE ('Deconv', BMFILE, ' ')
      CALL IMGCLONE ('Image', 'LDS')
C
C Perform linear deconvolution
C         
      CALL IMGFFT ('Deconv', 'XFR')
      CALL IMGCLONE ('XFR', 'MVis')
      CALL ARRSTAT ('XFR', ' ')
      MIND = MIND * DATFGETR ('XFR', 'ARRMAX')
      CALL IMGLDEC ('Image', 'XFR', MIND, 'LDS', 'MVis')
C
C Smooth if required
C
      IF (BEAM(1).GT.0.0) THEN
         CALL IMGSMOOT ('LDS', BEAM, 'LDS', 'XFR')
         CALL IMGP2PB ('LDS', BEAM, 'LDS')
         CALL DATPUTC ('LDS', 'BUNIT', 'JY/BEAM', 1)
      END IF
C
C Write image
C
      CALL FILIMGPU ('LDS', LDSFILE, ' ')
C
 999  END
