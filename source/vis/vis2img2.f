C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vis2img2.f	1.1	 3/14/91
C
      SUBROUTINE VIS2IMG2 (VIS1, VIS2, SUB, IMG, GRID1, GRID2, PSF)
C
C Transform a visibility data set. The output image must exist before 
C calling. The visibility data must be in a standard form. PSF
C determines whether a point spread function is made INSTEAD of
C the usual gridded data. The data will be phase shifted as necessary
C in order to line up the visibility data with the image. This routine
C can handle 1, 2, 3D grids and it can do 3D degridding via a slow 
C transform in Z.
C
C This version differs from VISTOIMG in that it accepts two data streams,
C typically AC and BD, and adds then into one image.
C
C
C	VIS1	CH*(*)	input	Name of visibility set for AC
C	VIS2	CH*(*)	input	Name of visibility set for BD
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C	IMG	CH*(*)	input	Name of output image
C	GRID1	CH*(*)	input	Name of grid for gridding AC and result
C	GRID2	CH*(*)	input	Name of grid for gridding BD (temporary)
C	PSF	LOG	input	TRUE for PSF calculation
C Audit trail:
C	New version
C				R. T. Duquet	May 18 1990
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS1, VIS2, SUB, IMG, GRID1, GRID2
      LOGICAL		PSF
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VIS2IMG2')
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
      CALL DATGETC (GRID1, 'CTYPE', ITYPE, SYSMXDIM, NDUMMY)
      SLOWZ = ITYPE(3).EQ.'N----SIN'
      CALL DATPUTL (IMG, 'SLOWZ', SLOWZ, 1)
C
C Now do the work
C
      CALL VISGRID (VIS1, SUB, GRID1, PSF)
      CALL VISGRID (VIS2, SUB, GRID2, PSF)
      CALL ARRADD (GRID1, GRID2, GRID1) 
      CALL ARRSCALE(GRID1,0.5,0.0,GRID1)
      CALL IMGFFT (GRID1, IMG)
      IF(DATEXIST(STRM2(GRID1, 'CFTYPE'))) THEN
         CALL DATGETC (GRID1, 'CFTYPE', CFTYPE, 1, NDUMMY)
         CALL DATPUTC (IMG, 'CFTYPE', CFTYPE, 1)
      END IF
      CALL IMGGRIDC (IMG, IMG, 'CORRECT')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END








