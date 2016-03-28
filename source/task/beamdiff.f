C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fitbeam.f	1.1    9/25/93
C
      SUBROUTINE SDEMAIN
C
CD Program to fit a gaussian to a beam
C
C Audit trail:
C	Original version
C				M.A. Hodlaway	June 9, 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'BEAMDIFF')
C
      CHARACTER*(SYSMXNAM)	PSFFILE, DIFFFILE
      INTEGER		NDUMMY
      REAL		LEVEL, LEVEL2, SCALE, BEAM(4), SUMDIFF, SUM, D
      REAL		DATFGETR
C==================================================================
C
      CALL MSGWELCO ('I get diff betwn PSF and Gaussian')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('DIFF', DIFFFILE, 1, NDUMMY)
      CALL USRGETR ('Level', LEVEL, 1, NDUMMY)
C
      IF (LEVEL .GE. 1.0 .OR. LEVEL .LE. 0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $        ' 0.0 < Level < 1.0')
         GOTO 999
      ENDIF
C
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
C
      CALL MSGPUT ('Non-Linear Beam Fit:','I')
      CALL DATPUTC ('PSF','FIT-ALG','NONLINEAR',1)
      CALL IMGBMSHP( 'PSF' )
C
      CALL IMGCLONE('PSF', 'GAUSS')
      CALL ARRSETCO ('GAUSS', 0.0, 0.0)
      CALL IMGMODEL ('GAUSS', 1, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     $     'POIN')
      BEAM(1) = DATFGETR ('PSF', 'BMAJ')*3600.0
      BEAM(2) = DATFGETR ('PSF', 'BMIN')*3600.0
      BEAM(3) = DATFGETR ('PSF', 'BPA')
      BEAM(4) = 0.0
C
      CALL IMGSMOOT('GAUSS', BEAM, 'GAUSS', 'Work')
      CALL IMGP2PB ('GAUSS', BEAM, 'GAUSS')
      CALL DATDELET ('Work')
C
      LEVEL2 = LEVEL+0.001
      CALL ARRLC ('GAUSS', 1.0, 'PSF', -1.0, 'DIFF')
      CALL ARRCLIP ('GAUSS', LEVEL, LEVEL2, 'MASK')
      CALL IMGCLONE ('MASK', 'BACKGROUND')
      CALL ARRSETCO ('BACKGROUND', 0.0, LEVEL)
      CALL ARRLC ('MASK', 1.0, 'BACKGROUND', -1.0, 'MASK')
      CALL DATDELET ('BACKGROUND')
      SCALE = 1.0/(LEVEL2 - LEVEL)
      CALL ARRSCALE ('MASK', SCALE, 0.0, 'MASK')
      CALL ARRMULT  ('DIFF', 'MASK', 'DIFF')
C
      IF (DIFFFILE .NE. ' ') THEN
         CALL FILIMGPU ('DIFF', DIFFFILE, ' ')
      ENDIF
      CALL ARRSUM   ('DIFF', SUMDIFF)
      CALL ARRSUM   ('GAUSS', SUM)
      D = SUMDIFF / SUM
      WRITE (MESSAGE, 1010) D
 1010 FORMAT ('Fractional Difference: ',F10.5)
      CALL MSGPUT (MESSAGE, 'I')
C
 999  CONTINUE
      END
