C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgconv.f	1.4    12/1/92
C
      SUBROUTINE SDEMAIN
C
CD Program to convolve with a point spread function
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C       Turned on flag to allow exact FFT computations
C				D.S.Briggs	30 Mar 92
C       Force image to size of PSF, if requested
C				D.S.Briggs	1 May 92
C	Missed an exact FFT flag. Fixed.
C				D.S.Briggs	1 Dec 93
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCONV')
C
      CHARACTER*(SYSMXNAM)	PSFFILE, INFILE, OUTFILE
      LOGICAL			DOFIT
      INTEGER		NDUMMY
C==================================================================
      CALL MSGWELCO ('I convolve images')
      CALL USRCTL
C
C Get input images
C
      CALL USRGETC('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC('Image', INFILE, 1, NDUMMY)
      CALL USRGETC('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETL('Fit', DOFIT, 1, NDUMMY)
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
C
      IF (DOFIT) THEN
         CALL IMGFITTO ('Image', 'PSF', 'TempImage')
         CALL DATDELET ('Image')
         CALL DATRENAM ('TempImage', 'Image')
      END IF
      CALL DATPUTC ('PSF', 'FFTSIZE', 'EXACT', 1)
      CALL IMGMAKEX ('PSF', 'XFR')
      CALL DATDELET ('PSF')
C
C Clone some images to get convenient sizes. 
C
      CALL IMGCLONE ('Image', 'Output')
      CALL IMGCLONE ('XFR', 'Modvis')
C
C Now calculate convolution
C
      CALL DATPUTC ('Image', 'FFTSIZE', 'EXACT', 1)
      CALL IMGFFT ('Image', 'Modvis')
      CALL ARRMULT ('Modvis', 'XFR', 'Modvis')
      CALL DATPUTC ('Modvis', 'FFTSIZE', 'EXACT', 1)
      CALL IMGFFT ('Modvis', 'Output')
C
C Output result
C
      CALL FILIMGPU ('Output', OUTFILE, ' ')
C
      END
