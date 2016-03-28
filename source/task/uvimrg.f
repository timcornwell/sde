C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)uvimrg.f	1.1    6/7/93
C
      SUBROUTINE SDEMAIN
C
CD Combine two images in the Fourier plane
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S. Briggs	22 Jan 93
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UVIMRG')
C
      CHARACTER*(SYSMXNAM) 	INFILE1, INFILE2, WTFILE, OUTFILE
      REAL		WMAX, WMIN
      INTEGER		NDUMMY, ODIR
C
      REAL		DATFGETR
C==================================================================
      CALL MSGWELCO ('I merge images in the Fourier plane')
      CALL USRCTL
C
C Get Images
C
      CALL USRGETC ('Image1', INFILE1, 1, NDUMMY)
      CALL USRGETC ('Image2', INFILE2, 1, NDUMMY)
      CALL USRGETC ('uvWeight', WTFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
C
      CALL FILIMGGE ('Image1', INFILE1, ' ')
      CALL FILIMGGE ('Image2', INFILE2, ' ')
      CALL FILIMGGE ('uvWeight', WTFILE, ' ')
C
      CALL ARRSTAT ('uvWeight', ' ')
      WMIN = DATFGETR ('uvWeight', 'ARRMIN')
      WMAX = DATFGETR ('uvWeight', 'ARRMAX')
      WRITE (MESSAGE, 1000) WMIN, WMAX
 1000 FORMAT ('Weight min, max =', F9.5,F12.5)
      CALL MSGPUT (MESSAGE,'I')
C
      CALL DATPUTC ('uvWeight', 'FFTSIZE', 'EXACT', 1)
      CALL DATPUTC ('Image1', 'FFTSIZE', 'EXACT', 1)
      CALL DATPUTC ('Image2', 'FFTSIZE', 'EXACT', 1)
C
      CALL FFTCONJA ('uvWeight', 'Template', ODIR, 0)
      CALL DATRENAM ('Image1', 'Temp')
      CALL IMGFITTO ('Temp', 'Template', 'Image1')
      CALL DATDELET ('Temp')
      CALL DATRENAM ('Image2', 'Temp')
      CALL IMGFITTO ('Temp', 'Template', 'Image2')
      CALL DATDELET ('Temp')
      CALL DATDELET ('Template')
C
      CALL IMGFFT ('Image1', 'XImage1')
      CALL IMGFFT ('Image2', 'XImage2')
C
      CALL IMGCLONE ('XImage1', 'XOutput')
      CALL ARRCVTX ('uvWeight', 'XWeight1')
      CALL ARRSCALE ('uvWeight', -1.0, 1.0, 'uvWeight2')
      CALL ARRCVTX ('uvWeight2', 'XWeight2')
C
      CALL ARRMULT ('XImage1', 'XWeight1', 'XImage1')
      CALL ARRMULT ('XImage2', 'XWeight2', 'XImage2')
      CALL ARRADD ('XImage1', 'XImage2', 'XOutput')
C
      CALL DATPUTC ('XOutput', 'FFTSIZE', 'EXACT', 1)
      CALL IMGFFT ('XOutput', 'Output')
C
C Write result 
C
      CALL FILIMGPU('Output',OUTFILE,' ')
C
 999  CONTINUE
      END
