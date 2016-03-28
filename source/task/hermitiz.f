C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hermitiz.f	1.1	 6/30/94
C
      SUBROUTINE SDEMAIN
C
CD Program to turn half planes into full hermitian planes
C  If the reference pixel is not at IX=1, this may fail.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C					M.A. Holdaway	June 30, 1994
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HERMITIZ')
C
      INTEGER 		NDUMMY
      CHARACTER*(SYSMXNAM)	INFILE, OUTFILE
C
C==================================================================
C
      CALL MSGWELCO ('I turn half planes into Hermitian full planes')
      CALL USRCTL
C
      CALL USRGETC ('In', INFILE, 1, NDUMMY)
      CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
      CALL FILIMGGE ('Image', INFILE, ' ')
C
      CALL IMGHERMI ('Image', 'Hermit')
C
C Put the data on Disk
C
      CALL FILIMGPU ('Hermit', OUTFILE, ' ')
C
 999  CONTINUE
      END
