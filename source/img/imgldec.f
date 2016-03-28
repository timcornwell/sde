C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgldec.f	1.4	 7/20/92
C
      SUBROUTINE IMGLDEC (IMG, XFR, MIND, LD, MVIS)
C
C Do linear deconvolution.
C
C
C	IMG	CH*(*)	input	Name of image to be deconved, etc.
C	XFR	CH*(*)	input	Name of transfer function
C      MIND    REAL    input   Minimum denominator
C	LD	CH*(*)	input	Name of output image
C	MVIS	CH*(*)	input	Name of work array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed to do complex division
C				T.J. Cornwell	Nov 21 1989
C
C---------------------------------------------------------------------
#include		"stdinc.h"
C
C
      CHARACTER*(*)	IMG, LD, XFR, MVIS
      REAL		MIND
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGLDEC')
C
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL IMGFFT (IMG, MVIS)
      CALL ARRCDIV (MVIS, XFR, MIND, MVIS)
      CALL IMGFFT (MVIS, LD)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
