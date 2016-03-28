C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tcorrpl.f	1.3    11/7/90
C
      SUBROUTINE SDEMAIN
CD     
CD    Program to simulated photon limited triple correlations 
CD     of a number of frames
C
CS    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from tcorr
C                                         R.G. Marson     Sep 22 1990
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TCORRPL')
C     
      CHARACTER*(SYSMXNAM) INFILE, OUTFILE
      INTEGER           NDUMMY, NFRAMES, I
C     
C==================================================================
C     
      CALL MSGWELCO ('I simulate  photon limited triple products')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get the Image file
C
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL FILIMGGE('Image', INFILE, ' ')
C
C Get the number of frames
C 
      CALL USRGETI('Frames', NFRAMES, 1, NDUMMY)
      NFRAMES = MAX (1, NFRAMES)
C
C loop over the number of frames
C
      DO I = 1, NFRAMES
C
C Add photon noise to the image
C
         CALL ARRPOISS ('Image', 'Photon', 65)
C
C Triple-correlate it
C
         CALL ARR3CORR('Photon', 'Photon', 'Triple')
C
C Add it together to the accumulated sum
C
         IF (I.EQ.1) THEN
            CALL ARRCOPY('Triple', 'Sum')
         ELSE
            CALL ARRADD('Triple', 'Sum', 'Sum')
         END IF
      END DO
C
C And write out the file
C
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
      CALL FILIMGPU('Sum', OUTFILE, ' ')
C
 999  CONTINUE
      END
