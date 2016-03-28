C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgslice.f	1.3    5/31/94
C
      SUBROUTINE IMGSLICE (IMG, BLC, TRC, NP, ORIGIN, ORDER, HANDLE)
C
CD Slice through an image
C
C	IMG	CH*(*)		input	Name of image
C	BLC	REAL*(*)	input	Start of slice
C	TRC	REAL*(*)	input	End of slice
C	NP	INT		input	Number of points in slice
C	ORIGIN	REAL		input	Slice abscissa at BLC
C	ORDER	INT		input	Interpolator order (sort of)
C	HANDLE	CH*(*)		input	Handle for output stream
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Feb 26 1994
C	Added comment character '#' to header lines, and fixed coordinates
C	for non-skyplane images
C				D.S.Briggs	May 24 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, HANDLE
      REAL		BLC(SYSMXDIM), TRC(SYSMXDIM), ORIGIN
      INTEGER		NP, ORDER
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSLICE')
C
      CHARACTER		ATYPE*1
      CHARACTER*8	CTYPE(8)
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD
      REAL		CDELT(SYSMXDIM), SX, SY
C
      INTEGER		NREAL, NDUMMY
C
      LOGICAL		DATEXIST
      INTEGER		CRDRNAX
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IMG, NAX, NAXIS, ATYPE, ADD)
      IF (ERROR) GO TO 990
C
      IF (ATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//ATYPE//' for Image')
         GO TO 990
      END IF
C
      NREAL = CRDRNAX (NAX, NAXIS)
      IF (NREAL.NE.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Can only slice real images')
         GO TO 990
      END IF
C
      IF (DATEXIST(STRM2(IMG,'CTYPE'))) THEN
         CALL DATGETC (IMG, 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
         CALL DATGETR (IMG, 'CDELT', CDELT, SYSMXDIM, NDUMMY)
         IF (CTYPE(1)(1:2).EQ.'RA') THEN
            MESSAGE = '#    Distance ''''      Value      No.'
            SX = CDELT(1) * 3600.0
            SY = CDELT(2) * 3600.0
         ELSE IF (CTYPE(1)(1:2).EQ.'UU') THEN
            MESSAGE = '#    Distance (lambda)      Value      No.'
            SX = CDELT(1)
            SY = CDELT(2)
         ELSE
            MESSAGE = '#    Distance (' // CTYPE(1) //
     $         ')      Value      No.'
            SX = CDELT(1)
            SY = CDELT(2)
         END IF
      ELSE
         CALL MSGPUT ('Missing coordinates -- output is in pixels','W')
         MESSAGE = '#    Distance (pix)   Value      No.'
         SX = 1.0
         SY = 1.0
      END IF
      CALL TXTWRITE ('Output', MESSAGE)
C
      IF ((BLC(1).LT.0.5).OR.(BLC(1).GT.NAXIS(1)+0.5) .OR.
     $    (BLC(2).LT.0.5).OR.(BLC(2).GT.NAXIS(2)+0.5) .OR.
     $    (TRC(1).LT.0.5).OR.(TRC(1).GT.NAXIS(1)+0.5) .OR.
     $    (TRC(2).LT.0.5).OR.(TRC(2).GT.NAXIS(2)+0.5)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Bad limits for slice')
         GO TO 990
      END IF
C
      CALL PIXRSL2D (MEMR(ADD), NAXIS(1), NAXIS(2), BLC(1), TRC(1),
     $   BLC(2), TRC(2), NP, ORIGIN, SX, SY, ORDER, HANDLE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

