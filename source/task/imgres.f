C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgres.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to find residuals
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGRES')
C
      CHARACTER*(SYSMXNAM)	DRTFILE, PSFFILE, MODFILE, RESFILE
      INTEGER		NDUMMY
C==================================================================
      CALL MSGWELCO ('I find residuals')
      CALL USRCTL
C
C Get input images
C
      CALL USRGETC('Dirty', DRTFILE, 1, NDUMMY)
      CALL USRGETC('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC('Residuals', RESFILE, 1, NDUMMY)
      CALL FILIMGGE ('Dirty', DRTFILE, ' ')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
      CALL FILIMGGE ('Model', MODFILE, ' ')
C
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL DATDELET ('PSF')
C
C Clone some images to get convenient sizes. The dirty image
C and the residual image can be any size at all. 
C
      CALL IMGCLONE ('Dirty', 'Residuals')
      CALL IMGCLONE ('XFR', 'Modvis')
C
C Now calculate residuals
C
      CALL IMGRESID ('Model', 'Dirty', 'XFR', 'Residuals', 'Modvis')
C
C Output result
C
      CALL FILIMGPU ('Residuals', RESFILE, ' ')
C
 999  CONTINUE
      END
