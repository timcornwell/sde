C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)curv.f	1.1 7/15/92
C
      SUBROUTINE SDEMAIN
C
CD Program to form second derivative of an image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	July 15 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CURV')
C
      INTEGER 		NDUMMY
      CHARACTER*(SYSMXNAM)	INFILE, SMFILE
C==================================================================
      CALL MSGWELCO ('I form the curvature image of an image')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', SMFILE, 1, NDUMMY)
C
C Get input Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL IMGCLONE ('Image', 'Curv')
C
      CALL IMGCURV ('Image', 'Curv', 'Swork')
C
C Write out answer
C
      CALL FILIMGPU ('Curv', SMFILE, ' ')
C
 999  CONTINUE
      END
