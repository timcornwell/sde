C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpb.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to treat primary beam patterns
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
      PARAMETER		(ROUTINE = 'IMGPB')
C
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE, MODE, TELESCOP
      REAL		TELDIAM
      INTEGER		NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I apply or correct primary beams')
      CALL USRCTL
C
C Get mode of operation
C
      CALL USRGETC ('Mode', MODE, 1, NDUMMY)
      CALL USRGETC ('Telescop', TELESCOP, 1, NDUMMY)
      CALL USRGETR ('Teldiam', TELDIAM, 1, NDUMMY)
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
      IF(TELESCOP.NE.' ') THEN
         CALL DATPUTC ('Image', 'TELESCOP', TELESCOP, 1)
         CALL DATPUTR ('Image', 'TELDIAM', TELDIAM, 1)
      END IF
      CALL IMGCLONE ('Image', 'Output')
      CALL IMGPB ('Image', 'Output', MODE)
C
C Write result 
C
      CALL USRGETC('Output', OUTFILE, 1, NDUMMY)
      CALL FILIMGPU('Output', OUTFILE, ' ')
C
 999  CONTINUE
      END
