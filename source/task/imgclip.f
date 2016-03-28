C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgclip.f	1.4    5/3/93
C
      SUBROUTINE SDEMAIN
C
CD Program to clip an image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Scale and offset added prior to clip
C				D.S.Briggs	Mar 4 1992
C	Added ZERO to (sometimes) set pixels to zero instead 
C	of clipping to MIN or MAX
C				M.A. Holdaway	May 3 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCLIP')
C
      CHARACTER*(SYSMXNAM) 	INFILE,
     2                          OUTFILE
      INTEGER		NDUMMY
      REAL		IMGMAX, IMGMIN, SCALE, OFFSET, ZERO
C==================================================================
C
      CALL MSGWELCO ('I clip images')
      CALL USRCTL
      CALL USRGETR ('Max', IMGMAX, 1, NDUMMY)
      CALL USRGETR ('Min', IMGMIN, 1, NDUMMY)
      CALL USRGETR ('Scale', SCALE, 1, NDUMMY)
      CALL USRGETR ('Offset', OFFSET, 1, NDUMMY)
      CALL USRGETR ('Zero', ZERO, 1, NDUMMY)
C
C Get Image
C
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL FILIMGGE ('Image', INFILE, ' ')
C
C Call main routine
C
      CALL IMGCLONE ('Image', 'Output')
      CALL ARRSCALE ('Image', SCALE, OFFSET, 'Image')
      IF (ZERO .EQ. 0.0) THEN
         CALL ARRCLIP ('Image', IMGMIN, IMGMAX, 'Output')
      ELSE IF (ZERO .GT. 0.0) THEN
         CALL ARRCLIP2 ('Image', 'Image', IMGMIN, 0.0,
     $      IMGMAX, IMGMAX, 'Output')
      ELSE IF (ZERO .LT. 0.0) THEN
         CALL ARRCLIP2 ('Image', 'Image', IMGMIN, IMGMIN,
     $      IMGMAX, 0.0, 'Output')
      ENDIF
C
C Write result 
C
      CALL USRGETC('Output', OUTFILE, 1, NDUMMY)
      CALL FILIMGPU('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
