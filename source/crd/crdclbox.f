C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdclbox.f	1.1    5/19/92
C
      SUBROUTINE CRDCLBOX (IN, BLC, TRC)
C
CD Clip a box to less than or equal to size of image.
C
C	IN	CH*(*)	input	Name of image
C	BLC	INT	in/out	BLC
C	TRC	INT	in/out	TRC
C 
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	10 May 1992
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	IN
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDCLBOX')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), NDUMMY
      CHARACTER		ATYPE*1
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, NAX, NAXIS, ATYPE, NDUMMY)
      DO 50 IAX = NAX+1, SYSMXDIM
         NAXIS(IAX) = 1
 50   CONTINUE
C
      DO 100 IAX = 1, SYSMXDIM
         BLC(IAX) = MIN(MAX(1, BLC(IAX)), NAXIS(IAX))
         TRC(IAX) = MAX(BLC(IAX), MIN(TRC(IAX), NAXIS(IAX)))
 100  CONTINUE
C
 999  CONTINUE
      END
