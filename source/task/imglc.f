C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imglc.f	1.3    5/7/92
C
      SUBROUTINE SDEMAIN
C
CD Program to make a linear combination of two images
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGLC')
C
      CHARACTER*(SYSMXNAM) 	INFILE1,
     1				INFILE2,
     2                          OUTFILE
      INTEGER		NDUMMY
      REAL       	W1,W2
      LOGICAL		DOFIT
C==================================================================
C
      CALL MSGWELCO ('I add images')
      CALL USRCTL
C
C Get Images
C
      CALL USRGETC ('Image1', INFILE1, 1, NDUMMY)
      CALL FILIMGGE ('Image1', INFILE1, '*')
      CALL USRGETC ('Image2',INFILE2, 1, NDUMMY)
      CALL FILIMGGE ('Image2', INFILE2, '*')
C
C Get Weights
C
      CALL USRGETR('Weight1',W1,1,NDUMMY)
      CALL USRGETR('Weight2',W2,1,NDUMMY)
      CALL USRGETC('Output',OUTFILE,1,NDUMMY)
C
C Fit to image1
C
      CALL USRGETL('Fit',DOFIT,1,NDUMMY)
      IF (DOFIT) THEN
         CALL IMGFITTO ('Image2', 'Image1', 'TempImage')
         CALL DATDELET ('Image2')
         CALL DATRENAM ('TempImage', 'Image2')
      END IF
C      
C Call main routine to make a linear combination
C
      CALL IMGCLONE ('Image1', 'Output')
      CALL ARRLC ('Image1',W1,'Image2',W2,'Output')
C
      CALL HISOPEN ('Output')
      CALL HISINPUT ('Output')
C
C Write result 
C
      CALL FILIMGPU('Output',OUTFILE,' ')
C
 999  CONTINUE
      END
