C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imshadow.f	1.1	 3/31/94
C
      SUBROUTINE SDEMAIN
C
CD Program to make an image of what is shadow from some illumination point
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	April 1, 1994
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMSHADOW')
C
      CHARACTER*(SYSMXNAM) 	IMAGE, SHADIN, SHADOUT
      REAL			VIEW(3), HEIGHT
C
      INTEGER		NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I find shadows on an image')
      CALL USRCTL
C
      CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
      CALL USRGETC ('ShadowIn', SHADIN, 1, NDUMMY)
      CALL USRGETC ('ShadowOut', SHADOUT, 1, NDUMMY)
      CALL USRGETR ('ViewPoint', VIEW, 3, NDUMMY)
      CALL USRGETR ('Height', HEIGHT, 1, NDUMMY)
C
      CALL FILIMGGE ('Image', IMAGE, '*')
      IF (SHADIN .NE. ' ') THEN
         CALL FILIMGGE ('Shadow', SHADIN, '*')
C
C Fit to image
C
         CALL IMGFITTO ('Shadow', 'Image', 'TempImage')
         CALL DATDELET ('Shadow')
         CALL DATRENAM ('TempImage', 'Shadow')
      ELSE
         CALL IMGCLONE ('Image', 'Shadow')
         CALL ARRSETCO ('Shadow', 0.0, 0.0)
      ENDIF
C
      CALL ARRSHADO ('Image', VIEW, HEIGHT, 'Shadow')
C      
      CALL FILIMGPU('Shadow', SHADOUT,' ')
C
 999  CONTINUE
      END
