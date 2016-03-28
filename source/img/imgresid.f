C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgresid.f	1.3    11/7/90
C
      SUBROUTINE IMGRESID (MODEL, DIRTY, XFR, RESID, MODVIS)
C
CD Find residuals for a given model: defined by DIRTY-PSF*MODEL
C
C	MODEL	CH*(*)	input	Name of model image
C	DIRTY	CH*(*)	input	Name of dirty image
C	XFR	CH*(*)	input	Name of XFR
C	RESID	CH*(*)	input	Name of residual image
C	MODVIS  CH*(*)	input	Name of output model visibility
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, DIRTY, XFR, RESID, MODVIS
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGRESID')
C
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL IMGFFT (MODEL, MODVIS)
      CALL ARRMULT (MODVIS, XFR, MODVIS)
      CALL IMGFFT (MODVIS, RESID)
      CALL ARRSUBTR (DIRTY, RESID, RESID)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
