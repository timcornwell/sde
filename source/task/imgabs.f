C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgabs.f	1.3    6/5/93
C
      SUBROUTINE SDEMAIN
C
CD Program to take absolute value of an image
C
C	Added * default for output file
C				D.S.Briggs	Apr 19 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGABS')
C
      CHARACTER*(SYSMXNAM) 	INFILE,
     2                          OUTFILE
      INTEGER		NDUMMY
C
      INTEGER		STRLEN
C==================================================================
C
      CALL MSGWELCO ('I take absolute values of images')
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
C
C Get Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
C
      CALL ARRABS ('Image', 'Output')
      CALL HEDCOPY ('Image', 'Output')
      CALL HISCOPY ('Image', 'Output')
C
C Write result 
C
      IF (OUTFILE.EQ.'*') OUTFILE = INFILE(1:STRLEN(INFILE)) // '.ABS'
      CALL FILIMGPU ('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
