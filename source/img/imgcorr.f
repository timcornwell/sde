C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgcorr.f	1.3    11/7/90
C
      SUBROUTINE IMGCORR (MODEL, PSF, XFR, CONV, MODVIS)
C
CD Correlate model by PSF corresponding to a XFR, If PSF is non-blank
C then the XFR is made
C
C	MODEL	CH*(*)	input	Name of model image
C	PSF	CH*(*)	input	Name of PSF 
C	XFR	CH*(*)	input	Name of XFR
C	CONV	CH*(*)	input	Name of residual image
C	MODVIS  CH*(*)	input	Name of output model visibility
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, PSF, XFR, CONV, MODVIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCORR')
C
C==================================================================
      IF (ERROR) GO TO 999
C
      IF (PSF.NE.' ') THEN
         CALL IMGMAKEX (PSF, XFR)
      END IF
      CALL IMGFFT (MODEL, MODVIS)
      CALL ARRCMULT (MODVIS, XFR, MODVIS)
      CALL IMGFFT (MODVIS, CONV)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
