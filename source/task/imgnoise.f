C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgnoise.f	1.1    8/12/93
C
      SUBROUTINE SDEMAIN
C
CD Program to add noise to an image
C
C This currently only supports independent Gaussian noise.  
C Note that the task PHOTONS adds photon (poisson) noise
C
CD Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	10 Aug 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGNOISE')
C
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE
      REAL		RMS
      INTEGER		NDUMMY, SEED
C==================================================================
C
      CALL MSGWELCO ('I add noise to images')
      CALL USRCTL
C
C Get Image
C
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETR ('RMS', RMS, 1, NDUMMY)
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
C
      CALL FILIMGGE ('Image', INFILE, ' ')
C
      CALL MSGPUT ('Adding GAUSSIAN noise','I')
      CALL ARRGAUSS ('Image', 'Image', RMS, SEED)
C
      CALL DATRENAM ('Image', 'Output')
      CALL HISINPUT ('Output')
      CALL FILIMGPU ('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
