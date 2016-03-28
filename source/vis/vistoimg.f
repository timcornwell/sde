C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vistoimg.f	1.2    11/7/90
C
      SUBROUTINE VISTOIMG (VIS, SUB, IMG, GRID, PSF)
C
CD Transform a visibility data set. The output image must exist before 
C calling. The visibility data must be in a standard form. PSF
C determines whether a point spread function is made INSTEAD of
C the usual gridded data. The data will be phase shifted as necessary
C in order to line up the visibility data with the image. This routine
C can handle 1, 2, 3D grids and it can do 3D degridding via a slow 
C transform in Z.
C
C
C	VIS	CH*(*)	input	Name of visibility set
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C	IMG	CH*(*)	input	Name of output image
C	GRID	CH*(*)	input	Name of grid for gridding
C	PSF	LOG	input	TRUE for PSF calculation
C Audit trail:
C	New version
C				T.J. Cornwell	Feb 18 1989
C	Now copies CFTYPE
C				T.J. Cornwell	Feb 21 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, SUB, IMG, GRID
      LOGICAL		PSF
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISTOIMG')
C
      CHARACTER*8	ITYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	CFTYPE, STRM2
      LOGICAL		DATEXIST, SLOWZ
C
      INTEGER		NDUMMY
C==========================================================================
      IF (ERROR) GO TO 999
C
C Find coordinate information
C
      CALL DATGETC (GRID, 'CTYPE', ITYPE, SYSMXDIM, NDUMMY)
      SLOWZ = ITYPE(3).EQ.'N----SIN'
      CALL DATPUTL (IMG, 'SLOWZ', SLOWZ, 1)
C
C Now do the work
C
      CALL VISGRID (VIS, SUB, GRID, PSF)
      CALL IMGFFT (GRID, IMG)
      IF(DATEXIST(STRM2(GRID, 'CFTYPE'))) THEN
         CALL DATGETC (GRID, 'CFTYPE', CFTYPE, 1, NDUMMY)
         CALL DATPUTC (IMG, 'CFTYPE', CFTYPE, 1)
      END IF
      CALL IMGGRIDC (IMG, IMG, 'CORRECT')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
