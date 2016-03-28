C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpower.f	1.1    2/21/91
C
      SUBROUTINE SDEMAIN
C
C Raise an image to a power
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPOWER')
C
C Names of input variables
C
      REAL		POWER, BLANK
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE
      INTEGER		NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I raise ine input image to some power')
C
      CALL USRCTL
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETR ('Power', POWER, 1, NDUMMY)
      CALL USRGETR ('Blank', BLANK, 1, NDUMMY)
      CALL FILIMGGE ('Image', INFILE, ' ')
C
      CALL ARRPOWER ('Image', POWER, BLANK, 'Image')
C
C Write result 
C
      CALL FILIMGPU('Image',OUTFILE,' ')
C
      END
