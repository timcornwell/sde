C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
      SUBROUTINE SDEMAIN
C
C Program to sub-section images
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSUB')
C
      INTEGER		NDUMMY
      CHARACTER*(SYSMXNAM)	INFILE, OUTFILE
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM),
     1			STEP(SYSMXDIM)
C==================================================================
      CALL MSGWELCO ('I sub-section images')
      CALL USRCTL
C
C Get input image
C
      CALL USRGETC ('In', INFILE, 1, NDUMMY)
      CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
      CALL FILIMGGE ('In', INFILE, ' ')
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL USRGETI ('STEP', STEP, SYSMXDIM, NDUMMY)
C
C Make window
C
      CALL DATCREAT ('Window')
      CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
      CALL IMGSUBSE ('In', 'Out', 'Window')
C
      CALL FILIMGPU ('Out', OUTFILE, ' ')
C
 999  CONTINUE
      END
