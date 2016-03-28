C
C       National Radio Astronomy Observatory, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)arrpro.f	1.1    8/27/92
C
      SUBROUTINE ARRPRO (A1, A2)
C
CD Promote images to common type
C
C	A1	CH*(*)	input	Name of array1
C	A2	CH*(*)	input	Name of array2
C
C Audit trail:
C       Original version:  Only complex promotion implemented.  More
C	flavors can be added if needed.
C                               D.S.Briggs      Aug 26 1992
C-----------------------------------------------------------------------
#include        "stdinc.h"
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'ARRPRO')
C
      CHARACTER*(*)     A1, A2
C
      INTEGER           NAX, NAXIS(SYSMXDIM), ADD1, ADD2
      CHARACTER*1	ATYPE1, ATYPE2
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, NAX, NAXIS, ATYPE1, ADD1)
      CALL DATGETAR (A2, NAX, NAXIS, ATYPE2, ADD2)
      IF (ERROR) GO TO 999
C
      IF (ATYPE1.EQ.ATYPE2) GO TO 999
C
      IF (ATYPE1.EQ.'X') THEN
         CALL ARRCVTX (A2, A2)
      ELSE IF (ATYPE2.EQ.'X') THEN
         CALL ARRCVTX (A1, A1)
      ELSE
         MESSAGE = 'Promotion of ' // ATYPE1 // ' and ' // ATYPE2 //
     $      ' not implemented'
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
