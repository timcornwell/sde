C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgrdclp.f	1.1	 6/10/91
C
      SUBROUTINE SDEMAIN
C
CD Clip an image to some value outside the annulus defines by MINRAD, MAXRAD
C
C Audit trail:
C	Original version: 
C				M.A. Holdaway	June 10 1991
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGRDCLP')
C
      CHARACTER*(SYSMXNAM) 	IMAGE, OUTPUT
      REAL			MAXRAD, MINRAD, CLIP
      INTEGER			NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I clip images outside the given radii')
      CALL USRCTL
C
C Get Image
C
      CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTPUT, 1, NDUMMY)
      CALL USRGETR ('Maxradius', MAXRAD, 1, NDUMMY)
      CALL USRGETR ('Minradius', MINRAD, 1, NDUMMY)
      CALL USRGETR ('ClipValue', CLIP, 1, NDUMMY)
      CALL FILIMGGE ('Image', IMAGE, ' ')
C
C Clip image outside MAXRAD
C
      MAXRAD = MAXRAD/3600.
      MINRAD = MINRAD/3600.
      CALL ARRRDCLP ('Image', MINRAD, MAXRAD, CLIP)
C
      CALL FILIMGPU ('Image', OUTPUT, ' ')
C
  999 CONTINUE
      END
