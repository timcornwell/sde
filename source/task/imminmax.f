C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imminmax.f	1.1	19 Mar 1994
C
      SUBROUTINE SDEMAIN
C
CD Program to take pixel by pixel min or max
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	March 18 1994
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMMINMAX')
C
      CHARACTER*(SYSMXNAM) 	INFILE1,
     1				INFILE2,
     2                          MINFILE, MAXFILE
      INTEGER		NDUMMY
      LOGICAL		DOFIT
C==================================================================
C
      CALL MSGWELCO ('I make min and max images')
      CALL USRCTL
C
C Get Images
C
      CALL USRGETC ('Image1', INFILE1, 1, NDUMMY)
      CALL USRGETC ('Image2',INFILE2, 1, NDUMMY)
      CALL USRGETL('Fit',DOFIT,1,NDUMMY)
      CALL USRGETC('Min',MINFILE,1,NDUMMY)
      CALL USRGETC('Max',MAXFILE,1,NDUMMY)
C
      CALL FILIMGGE ('Image1', INFILE1, '*')
      CALL FILIMGGE ('Image2', INFILE2, '*')
C
C
C Fit to image1
C
      IF (DOFIT) THEN
         CALL IMGFITTO ('Image2', 'Image1', 'TempImage')
         CALL DATDELET ('Image2')
         CALL DATRENAM ('TempImage', 'Image2')
      END IF
C      
C Call main routine to make a linear combination
C
      CALL IMGCLONE ('Image1', 'Min')
      CALL IMGCLONE ('Image1', 'Max')
      CALL ARRMINMA ('Image1', 'Image2', 'Min', 'Max')
C
      CALL HISOPEN ('Min')
      CALL HISINPUT ('Min')
      CALL HISOPEN ('Max')
      CALL HISINPUT ('Max')
C
C Write result 
C
      IF (MINFILE .NE. ' ') THEN
         CALL FILIMGPU('Min', MINFILE, ' ')
      ENDIF
      IF (MAXFILE .NE. ' ') THEN
         CALL FILIMGPU('Max', MAXFILE, ' ')
      ENDIF
C
 999  CONTINUE
      END
