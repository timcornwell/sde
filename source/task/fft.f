C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fft.f	1.2	 10/21/93
      SUBROUTINE SDEMAIN
C
CD FFT Anything to Anything, as long as its gridded
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 14 1990
C	Added FFTSIZE adverb
C				D.S.Briggs	Oct 21 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFT')
C
      INTEGER			NDUMMY, NAX, NAXIS(SYSMXDIM)
      REAL			RADD, XADD
      CHARACTER			ATYPE
      CHARACTER*(SYSMXNAM) 	IN, OUT, FFTSIZE
      LOGICAL			FULL
C==================================================================
C
      CALL MSGWELCO ('I FFT Real or Complex Images or Gridded VIS')
      CALL USRCTL
C
C Get Image
C
      CALL USRGETC ('IN', IN, 1, NDUMMY)
      CALL USRGETC ('OUT', OUT, 1, NDUMMY)
      CALL USRGETC ('FFTSize', FFTSIZE, 1, NDUMMY)
      CALL USRGETL ('Full', FULL, 1, NDUMMY)
C
      IF (FFTSIZE.EQ.' ') FFTSIZE = 'PWR2'
      IF ((FFTSIZE.NE.'PWR2').AND.(FFTSIZE.NE.'EXACT')) THEN
         CALL MSGPUT ('FFTSIZE forced to PWR2','I')
         FFTSIZE = 'PWR2'
      END IF
C
      CALL FILIMGGE ('Image', IN,  ' ')
C
      CALL DATGETAR ('Image', NAX, NAXIS, ATYPE, XADD)
      IF (ATYPE .NE. 'X'  .AND. FULL)
     $   CALL ARRCVTX ('Image', 'Image')
      IF (ERROR) GO TO 999
C
C Dump coordinates
C
      CALL MSGPUT ('Coordinates:', 'I')
      CALL CRDLIST ('Image')
C
      CALL DATPUTC ('Image', 'FFTSIZE', FFTSIZE, 1)
      CALL IMGFFT ('Image', 'Output')
      IF (ERROR) GOTO 999
C
      CALL FILIMGPU ('Output', OUT, ' ')
C
 999  CONTINUE
      END
