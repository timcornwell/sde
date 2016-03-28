C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgtovis.f	1.3    11/7/90
C
      SUBROUTINE IMGTOVIS (VIS, SUB, IMG, GRID, WKIMG)
C
CD Transform an image to vis data set. The output image must exist before 
C calling. The visibility data must be in a standard form.
C The visibility data will be phase shifted and the uvw's transformed
C in order to line up the visibility data with the image. This routine
C can handle 1, 2, 3D grids and it can do 3D gridding via a slow 
C transform in Z. The image is first expanded to a factor of two
C bigger (into WKIMG) and then transformed and de-gridded so there
C should be no significant aliasing errors.
C
C
C	VIS	CH*(*)	input	Name of visibility set
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C	IMG	CH*(*)	input	Name of output image
C	GRID	CH*(*)	input	Name of grid for gridding
C	WKIMG	CH*(*)	input	Name of work image: will be created
C				if necessary
C
C Audit trail:
C	New version
C				T.J. Cornwell	Feb 18 1989
C	Now copies CFTYPE from GRID
C				T.J. Cornwell	Feb 21 1989
C      Defaults on non-existence of CFTYPE
C                              T.J. Cornwell   March 27 1989
C      Now derives GRID from WKIMG rather than IMG, so that the grid
C      is a factor of two more finely sampled than before.
C                              T.J. Cornwell   April 4 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, SUB, IMG, GRID, WKIMG
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGTOVIS')
C
      CHARACTER*(SYSMXNAM)	CFTYPE, STRM2
      CHARACTER*8	ITYPE(SYSMXDIM)
      LOGICAL		SLOWZ, DATEXIST
C
      INTEGER		NDUMMY, DIR
C==========================================================================
      IF (ERROR) GO TO 999
C
C First expand the image
C
      CALL IMGDOUBL (IMG, WKIMG)
C
C Make the Grid if it does not exist: default transformation
C is slow in z
C
      IF(.NOT.DATEXIST(GRID)) THEN
         NDUMMY = 2
         CALL FFTCONJA (WKIMG, GRID, DIR, NDUMMY)
      END IF
C
C Find coordinate information
C
      CALL DATGETC (GRID, 'CTYPE', ITYPE, SYSMXDIM, NDUMMY)
      SLOWZ = ITYPE(3).EQ.'N----SIN'
C
C Get gridding info
C
      IF (DATEXIST (STRM2(GRID, 'CFTYPE'))) THEN
         CALL DATGETC (GRID, 'CFTYPE', CFTYPE, 1, NDUMMY)
      ELSE
         CFTYPE = 'SF'
      END IF
C
C Now do the work
C
      CALL DATPUTL (WKIMG, 'SLOWZ', SLOWZ, 1)
      CALL DATPUTC (WKIMG, 'CFTYPE', CFTYPE, 1)
      CALL IMGGRIDC (WKIMG, WKIMG, 'CORRECT')
      CALL IMGFFT (WKIMG, GRID)
      CALL VISDEGRI(VIS, SUB, GRID)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
