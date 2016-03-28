C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matimhdr.f	1.2    7/18/97
C
      SUBROUTINE MATIMHDR (MAT)
C
CD Fake up an "image" header for a matrix or vector
C
C	MAT	CH*(*)	input	Name of matrix
C
C For SAOimage to display matricies in the standard mathematical orientation
C (which is the whole point of this routine), you'll need to read it in
C with the "-rot 1" switch.
C
C Audit trail:
C	Initial version
C				D.S. Briggs	12 Nov 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MAT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MAT')
C
      CHARACTER*1	ATYPE
      INTEGER		I, NAX, RNAX, NAXIS(SYSMXDIM), ADD
C
      REAL		CELLSIZE(3), SHIFT(3)
C
      INTEGER		CRDRNAX
      DATA		CELLSIZE/3*1.0/, SHIFT/3*0.0/
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (MAT, NAX, NAXIS, ATYPE, ADD)
      RNAX = CRDRNAX(NAX,NAXIS)
      IF (ERROR) GO TO 990
C
      IF ((RNAX.GT.3).OR.(RNAX.LT.1)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Matrix has bad number of real axes')
         GO TO 999
      END IF
C
      DO 100 I = NAX+1,3
         NAXIS(I) = 1
 100  CONTINUE
C
      CALL DATRENAM (MAT,ROUTINE//'Temp')
      CALL IMGMAKE0 (MAT, NAXIS, CELLSIZE, SHIFT, 0.D0, 30.D0, 1.4D9,
     $   ATYPE)
      CALL ARRCOPY (ROUTINE//'Temp',MAT)
      CALL DATDELET (ROUTINE//'Temp')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
