C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to expand to a cube from an image
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
      PARAMETER		(ROUTINE = 'IMGSPH')
C
      CHARACTER*(SYSMXNAM) 	INFILE,
     2                          OUTFILE
      REAL		CELLZ
      INTEGER		NZ
      INTEGER		NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I expand images into spheres')
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
C Get control parameters
C
      CALL USRGETI ('Nz', NZ, 1, NDUMMY)
      CALL USRGETR ('Cellz', CELLZ, 1, NDUMMY)
      CELLZ = CELLZ/3600.0
C
C Call main routine
C
      CALL IMGSPH ('Image', 'Output', NZ, CELLZ)
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
