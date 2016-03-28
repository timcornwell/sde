C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdcheck.f	1.7	 7/23/92
C
      SUBROUTINE CRDCHECK (NAME1, NAME2)
C
CD Checks that two database entries have the same crds
C
C
C
C	NAME1	CH*(*)	input	Name of Image
C	NAME2	CH*(*)	input	Name of another Image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Feb 8 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME1, NAME2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDCHECK')
C
      LOGICAL		OK
      INTEGER		I, N
      INTEGER		NAX1,NAX2
      INTEGER		NAXIS1(SYSMXDIM), NAXIS2(SYSMXDIM)
      CHARACTER*8	TYPE1(SYSMXDIM), TYPE2(SYSMXDIM)
      REAL		ROTA1(SYSMXDIM), ROTA2(SYSMXDIM)
      REAL		DELT1(SYSMXDIM), DELT2(SYSMXDIM)
      REAL		RPIX1(SYSMXDIM), RPIX2(SYSMXDIM)
      DOUBLE PRECISION	RVAL1(SYSMXDIM), RVAL2(SYSMXDIM)
      INTEGER		STRLEN
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      OK = .TRUE.
      CALL CRDGET (NAME1, NAX1, TYPE1, NAXIS1, RVAL1, RPIX1, DELT1,
     1   ROTA1)
      CALL CRDGET (NAME2, NAX2, TYPE2, NAXIS2, RVAL2, RPIX2, DELT2,
     1   ROTA2)
      IF (NAX1 .NE. NAX2)  OK = .FALSE.
      N = MAX(NAX1, NAX2)
      DO 10 I = 1, N
         IF (NAXIS1(I) .NE. NAXIS2(I)) OK = .FALSE.
         IF (ROTA1(I)  .NE. ROTA2(I) ) OK = .FALSE.
         IF (TYPE1(I)  .NE. TYPE2(I) ) OK = .FALSE.
         IF (RVAL1(I)  .NE. RVAL2(I) ) OK = .FALSE.
         IF (RPIX1(I)  .NE. RPIX2(I) ) OK = .FALSE.
         IF (DELT1(I)  .NE. DELT2(I) ) OK = .FALSE.
 10   CONTINUE   
C
      IF (.NOT. OK) THEN
         CALL CRDLIST (NAME1)
         CALL CRDLIST (NAME2)               
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      ' images have different CRDs')
         GOTO 990
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
