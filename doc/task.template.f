C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C %W%    %G%
C
      SUBROUTINE SDEMAIN
C
C Program to make a linear combination of two images. This routine is
C called by the main program /sde/source/tsk/tsk.f and must be
C called SDEMAIN.
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TEMPLATE')
C
C Names of input variables
C
      REAL		W1, W2
      CHARACTER*(SYSMXNAM) 	INFILE1,
     1				INFILE2,
     2                          OUTFILE
      INTEGER		NDUMMY
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I am a template task')
C
C Call user interface routine, and set debug status
C
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get Images
C
      CALL USRGETC ('Image1', INFILE1, 1, NDUMMY)
      CALL FILIMGGE ('Image1', INFILE1, ' ')
C
      CALL USRGETC ('Image2',INFILE2, 1, NDUMMY)
      CALL FILIMGGE ('Image2', INFILE2, ' ')
C
C Get Weights
C
      CALL USRGETR('Weight1',W1,1,NDUMMY)
      CALL USRGETR('Weight2',W2,1,NDUMMY)
      CALL USRGETC('Output',OUTFILE,1,NDUMMY)
C
C Clone output image
C
      CALL IMGCLONE ('Image1', 'Output')
C
C Call main routine to make a linear combination
C
      CALL ARRLC ('Image1',W1,'Image2',W2,'Output')
C
C Write result 
C
      CALL FILIMGPU('Output',OUTFILE,' ')
C
      END
