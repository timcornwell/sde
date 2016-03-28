C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fitbeam.f	1.2    3/16/95
C
      SUBROUTINE SDEMAIN
C
CD Program to fit a gaussian to a beam
C
C Audit trail:
C	Original version
C				D.S.Briggs	Sept 23 1993
C	Display pixels per beam
C				D.S.Briggs	March 16 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FITBEAM')
C
      CHARACTER*(SYSMXNAM)	PSFFILE
      INTEGER		NDUMMY
      REAL		BEAM(4), PPB
C
      REAL		DATFGETR
C==================================================================
C
      CALL MSGWELCO ('I fit a Gaussian to a PSF')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
C
      CALL MSGPUT ('Old Style AIPS/SDE Beam Fit:','I')
      CALL DATPUTC ('PSF','FIT-ALG','ARCHIVE',1)
      CALL IMGBMSHP('PSF')
      CALL HEDBEMGE ('PSF', BEAM, ' ', 0.0)
      CALL IMGP2PB ('PSF', BEAM, ' ')
      PPB = DATFGETR('PSF', 'BPPB')
      WRITE (MESSAGE, 1000) PPB
 1000 FORMAT (F12.4,' pixels per beam area')
      CALL MSGPUT (MESSAGE, 'I')

      CALL MSGPUT ('Linear Beam Fit:','I')
      CALL DATPUTC ('PSF','FIT-ALG','LINEAR',1)
      CALL IMGBMSHP('PSF')
      CALL HEDBEMGE ('PSF', BEAM, ' ', 0.0)
      CALL IMGP2PB ('PSF', BEAM, ' ')
      PPB = DATFGETR('PSF', 'BPPB')
      WRITE (MESSAGE, 1000) PPB
      CALL MSGPUT (MESSAGE, 'I')

      CALL MSGPUT ('Non-Linear Beam Fit:','I')
      CALL DATPUTC ('PSF','FIT-ALG','NONLINEAR',1)
      CALL IMGBMSHP('PSF')
      CALL HEDBEMGE ('PSF', BEAM, ' ', 0.0)
      CALL IMGP2PB ('PSF', BEAM, ' ')
      PPB = DATFGETR('PSF', 'BPPB')
      WRITE (MESSAGE, 1000) PPB
      CALL MSGPUT (MESSAGE, 'I')
C
 999  CONTINUE
      END
