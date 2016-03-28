C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)slice.f	1.1    5/7/94
C
      SUBROUTINE SDEMAIN
C
CD Program to slice through an image
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Feb 26 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSUB')
C
      INTEGER		NDUMMY
      CHARACTER*(SYSMXNAM)	INFILE, OUTFILE
      REAL		BLC(SYSMXDIM), TRC(SYSMXDIM), ORIGIN
      INTEGER		NPOINTS, ORDER
C
      INTEGER		STRLEN
C==================================================================
      CALL MSGWELCO ('I take slices through images')
      CALL USRCTL
C
C Get input image
C
      CALL USRGETC ('In', INFILE, 1, NDUMMY)
      CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
      CALL USRGETR ('BLC', BLC, 3, NDUMMY)
      CALL USRGETR ('TRC', TRC, 3, NDUMMY)
      CALL USRGETI ('NPoints', NPOINTS, 1, NDUMMY)
      CALL USRGETI ('Order', ORDER, 1, NDUMMY)
      CALL USRGETR ('Origin', ORIGIN, 1, NDUMMY)
C
      CALL FILIMGGE ('Img', INFILE, ' ')
      MESSAGE = 'Opening ' // OUTFILE(1:STRLEN(OUTFILE)) //
     $   ' for WRITE as Output'
      CALL MSGPUT (MESSAGE, 'I')
      CALL FILDEL (OUTFILE)
      CALL TXTOPEN ('Output', OUTFILE, 'WRITE')
C
      CALL IMGSLICE ('Img', BLC, TRC, NPOINTS, ORIGIN, ORDER, 'Output')
C
      CALL TXTCLOSE ('Output')
C
 999  CONTINUE
      END

