C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsetco.f	1.1	 8/12/91
C
      SUBROUTINE SDEMAIN
C
C Program to make a linear combination of two images. This routine is
C called by the main program /sde/source/tsk/tsk.f and must be
C called SDEMAIN.
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
      PARAMETER		(ROUTINE = 'IMGSETCO')
C
C Names of input variables
C
      REAL		TOTAL, EACH
      CHARACTER*(SYSMXNAM) 	INFILE,
     2                          OUTFILE
      INTEGER		NDUMMY
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I set arrays to a constant value')
C
      CALL USRCTL
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETR ('Each', EACH, 1, NDUMMY)
      CALL USRGETR ('Total', TOTAL, 1, NDUMMY)
C
      CALL FILIMGGE ('Image', INFILE, ' ')
C
      CALL ARRSETCO ('Image', TOTAL, EACH)
C
C Write result 
C
      CALL FILIMGPU('Image',OUTFILE,' ')
C
      END
