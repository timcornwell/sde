C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgexp.f	1.1    6/16/93
C
      SUBROUTINE SDEMAIN
C
CD Program to exponentiate an image
C
C Audit trail:
C	Cloned from imgscale
C				R.G. Marson	June 16 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGEXP')
C
C Function Declarations
C   

C
C Local Variables
C
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE
      INTEGER		NDUMMY
C
C==================================================================
C
      CALL MSGWELCO ('I exponentiate images')
      CALL USRCTL
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
C
C Call main routine
C
      CALL IMGCLONE ('Image', 'Output')
      CALL ARREXP ('Image', 1.0, 0.0, 'Output')
C
C Write result 
C
      CALL HISINPUT('Output')
      CALL FILIMGPU('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
