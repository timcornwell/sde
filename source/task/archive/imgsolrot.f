C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to rotate the SUN
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
      PARAMETER		(ROUTINE = 'IMGSOLRO')
C
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE
      REAL		PHI
      INTEGER		NDUMMY
      REAL		RAD
      DATA		RAD	/960.0/
C==================================================================
C
      CALL MSGWELCO ('I rotate the SUN')
      CALL USRCTL
C
C Get rotation angle
C
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETR ('Phi', PHI, 1, NDUMMY)
      WRITE (MESSAGE, 1000) PHI
 1000 FORMAT ('Rotating by ',F9.3,' radians from left to right')
      CALL MSGPUT (MESSAGE, 'I')
C
C Get Image
C
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
      CALL IMGCLONE ('Image', 'Output')
      CALL DATPUTR ('Image', 'SOLRAD', RAD, 1)
      CALL IMGSOLRO ('Image', 'Output', PHI)
C
C Write result 
C
      CALL USRGETC('Output', OUTFILE, 1, NDUMMY)
      CALL FILIMGPU('Output', OUTFILE, ' ')
C
      END
