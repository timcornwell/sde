C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgdesph.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to collapse a cube to an image
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
      PARAMETER		(ROUTINE = 'IMGDESPH')
C
      CHARACTER*(SYSMXNAM) 	INFILE,
     2                          OUTFILE
      INTEGER		NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I project spheres')
      CALL USRCTL
C
C Get Image
C
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL FILIMGGE ('Image', INFILE, ' ')
C
C Dump coordinates
C
      CALL MSGPUT ('Coordinates for input Image:', 'I')
      CALL CRDLIST ('Image')
C
C Call main routine
C
      CALL IMGDESPH ('Image', 'Output')
C
C Dump coordinates
C
      CALL MSGPUT ('Coordinates for output image:', 'I')
      CALL CRDLIST ('Output')
C
C Write result 
C
      CALL USRGETC('Output', OUTFILE, 1, NDUMMY)
      CALL FILIMGPU('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
