C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgxfr.f	1.6    7/31/94
C
      SUBROUTINE SDEMAIN
C
CD Program to make a transfer function of an image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C       Enable exact FFTs
C				D.S.Briggs	May 22 1992
C	Added * default for output file
C				D.S.Briggs	Apr 19 1993
C	Added Full mode
C				D.S.Briggs	July 31 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGXFR')
C
      CHARACTER*(SYSMXNAM) 	INFILE,
     2                          OUTFILE
      INTEGER		NDUMMY
      LOGICAL		DOFULL
C
      INTEGER		STRLEN
C==================================================================
C
      CALL MSGWELCO ('I make a XFR function')
C
      CALL USRCTL
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETL ('Full', DOFULL, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
C
C Dump coordinates
C
      CALL MSGPUT ('Coordinates for input Image:', 'I')
      CALL CRDLIST ('Image')
C
      IF (DOFULL) THEN
         CALL ARRQU2X ('Image', 'NULL', 'TempComplex')
         CALL DATDELAR ('Image')
         CALL ARRCOPY ('TempComplex', 'Image')
         CALL DATDELET ('TempComplex')
      END IF
C
C Call main routine
C
      CALL DATPUTC ('Image', 'FFTSIZE', 'EXACT', 1)
      CALL IMGFFT ('Image', 'Tmp')
      CALL ARRABS ('Tmp', 'Output')
      CALL HEDCOPY ('Tmp', 'Output')
C
C Dump coordinates
C
      CALL MSGPUT ('Coordinates for output image:', 'I')
      CALL CRDLIST ('Output')
C
C Star default
C
      IF (OUTFILE.EQ.'*') OUTFILE = INFILE(1:STRLEN(INFILE)) // '.XFR'
C
C Write result
C
      CALL FILIMGPU ('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
