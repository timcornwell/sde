C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgcopy.f	1.5    2/8/95
C
      SUBROUTINE SDEMAIN
C
CD Program to copy an image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	TFORM keyword added for PGM files.
C				D.S.Briggs	Oct 29 1992
C	History keyword added, so one can disable copying
C	of enormous history files.
C				D.S.Briggs	Aug 8 1994
C	Add conversion to REAL if requested
C				D.S.Briggs	Feb 7 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCOPY')
C
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE, TFORM
      LOGICAL		DOHIS, DOCVRT
      INTEGER		NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I copy images')
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETL ('Real', DOCVRT, 1, NDUMMY)
      CALL USRGETL ('History', DOHIS, 1, NDUMMY)
      CALL USRGETC ('Tform', TFORM, 1, NDUMMY)
C
C Get Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      IF (TFORM.NE.' ')
     $   CALL DATPUTC ('Image', 'TFORM', TFORM, 1)
C
      CALL DATRENAM ('Image', 'Output')
C
      IF (DOCVRT) CALL ARRCVTR ('Output','Output')
C
C Write result 
C
      IF (.NOT.DOHIS) CALL HISCLOSE ('Output')
C
      CALL FILIMGPU ('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
