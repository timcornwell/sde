C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgstrim.f	1.1    5/19/92
C
      SUBROUTINE IMGSTRIM (IMAGE)
C
CD Trim an image to minimum support.
C
C	IMAGE	 	CH*(*)	input	Directory entry of input image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Mar 18 1992
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	IMAGE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSTRIM')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NDUMMY, I,
     $   		BBBLC(SYSMXDIM), BBTRC(SYSMXDIM)
      CHARACTER		ATYPE*1
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IMAGE, NAX, NAXIS, ATYPE, NDUMMY)
      CALL ARRSTAT(IMAGE, ' ')
      CALL DATGETI (IMAGE, 'ARRBBBLC', BBBLC, SYSMXDIM, NDUMMY)
      CALL DATGETI (IMAGE, 'ARRBBTRC', BBTRC, SYSMXDIM, NDUMMY)
C
C Do we really need to do this?
C
      DO 100 I = 1, NAX
         IF ((BBBLC(I).NE.1).OR.(BBTRC(I).NE.NAXIS(I))) GO TO 200
 100  CONTINUE
      RETURN
C
C An actual shrink is necessary
C
 200  CONTINUE
      CALL DATCREAT ('TrimWindow')
      CALL DATPUTI ('TrimWindow', 'TRC', BBTRC, SYSMXDIM)
      CALL DATPUTI ('TrimWindow', 'BLC', BBBLC, SYSMXDIM)
      CALL IMGSUBSE (IMAGE, 'TrimTemp', 'TrimWindow')
      CALL DATDELET (IMAGE)
      CALL DATDELET ('TrimWindow')
      CALL DATRENAM ('TrimTemp', IMAGE)
C
      CALL DATGETAR (IMAGE, NAX, NAXIS, ATYPE, NDUMMY)
      DO 300 I = 1, SYSMXDIM
         BBBLC(I) = 1
         BBTRC(I) = 1
 300  CONTINUE
C
      DO 310 I = 1, NAX
         BBTRC(I) = NAXIS(I)
 310  CONTINUE
C
      CALL DATPUTI (IMAGE, 'ARRBBBLC', BBBLC, SYSMXDIM)
      CALL DATPUTI (IMAGE, 'ARRBBTRC', BBTRC, SYSMXDIM)
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
