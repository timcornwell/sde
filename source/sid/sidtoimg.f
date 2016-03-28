C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)sidtoimg.f	1.2	 7/20/92
C
      SUBROUTINE SIDTOIMG (SID, SUB, IMG, GRID)
C
C Transform a single dish data set to an image. The image must exist
C before calling.
C
C
C	SID	CH*(*)	input	Name of single dish data set
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C	IMG	CH*(*)	input	Name of output image
C	GRID	CH*(*)	input	Name of grid for gridding
C Audit trail:
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	SID, SUB, IMG, GRID
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SIDTOIMG')
C
      CHARACTER*(SYSMXNAM)	CFTYPE, STRM2
      LOGICAL		DATEXIST
C
      INTEGER		NDUMMY
C==========================================================================
      IF (ERROR) GO TO 999
C
C Now do the work
C
      CALL SIDGRID (SID, SUB, IMG)
      CALL IMGFFT (IMG, GRID)
      IF(DATEXIST(STRM2(IMG, 'CFTYPE'))) THEN
         CALL DATGETC (IMG, 'CFTYPE', CFTYPE, 1, NDUMMY)
         CALL DATPUTC (GRID, 'CFTYPE', CFTYPE, 1)
      END IF
      CALL IMGGRIDC (GRID, GRID, 'CORRECT')
      CALL IMGFFT (GRID, IMG)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
