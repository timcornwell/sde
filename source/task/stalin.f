C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)stalin.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to strip history records
C
C Audit trail:
C	Since HISTORY cards are really read in by FTSREADH, we must
C	delete them explicitly.
C				T.J.Cornwell	Jan 7 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'STALIN')
C
      CHARACTER*(SYSMXNAM) 	INFILE,OUTFILE
      INTEGER                   NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I remove history cards')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
C
C Get input image
C
      CALL FILIMGGE ('In', INFILE, '*')
      IF (ERROR) GO TO 999
C
C Delete HII directory
C
      CALL HISCLOSE ('In')
C
C Put output image 
C
      CALL DATRENAM ('In', 'Out')
      CALL FILIMGPU('Out', OUTFILE, 'AIPS CC')
C
 999  CONTINUE
      END
