C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to add phton noise to an image
C
CS Arguments: CALL SDEMAIN
CA Audit trail:
CA	Original version: Audit trail comments go on this line
CA	and successive lines
CA				T.J.Cornwell	Jan 5 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PHOTONS')
C
      CHARACTER*(SYSMXNAM) 	INFILE,
     2                          OUTFILE
      REAL		SCALE
      INTEGER		NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I add photon noise to images')
      CALL USRCTL
C
C Get Image
C
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETR ('Scale', SCALE, 1, NDUMMY)
      CALL FILIMGGE ('Image', INFILE, ' ')
C
C Call main routine
C
      CALL ARRPOISS ('Image', SCALE, 'Output')
C
C Write result 
C
      CALL USRGETC('Output', OUTFILE, 1, NDUMMY)
      CALL FILIMGPU('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
