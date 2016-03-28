C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgconv0.f	1.1    6/7/93
C
      SUBROUTINE IMGCONV0 (MODEL, PSF, XFR, CONV, MODVIS)
C
CD Convolve model by PSF corresponding to a XFR, If PSF is non-blank
C then the XFR is made
C
C	MODEL	CH*(*)	input	Name of model image
C	PSF	CH*(*)	input	Name of PSF 
C	XFR	CH*(*)	input	Name of XFR
C	CONV	CH*(*)	input	Name of residual image
C	MODVIS  CH*(*)	input	Name of output model visibility
C Audit trail:
C	Cloned from IMGCONV.  The only difference is the liberal use
C	of exact FFT flags.
C				D.S.Briggs	4 Feb 93
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, PSF, XFR, CONV, MODVIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCONV0')
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL DATPUTC (PSF, 'FFTSIZE', 'EXACT', 1)
      CALL DATPUTC (MODEL, 'FFTSIZE', 'EXACT', 1)
      IF (PSF.NE.' ') THEN
         CALL IMGMAKEX (PSF, XFR)
      END IF
      CALL IMGFFT (MODEL, MODVIS)
      CALL ARRMULT (MODVIS, XFR, MODVIS)
      CALL DATPUTC (MODVIS, 'FFTSIZE', 'EXACT', 1)
      CALL IMGFFT (MODVIS, CONV)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
