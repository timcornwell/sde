C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by Associated Universities, Inc.; all rights reserved
C++
C @(#)crdchkig.f	1.2    7/23/92
C
      SUBROUTINE CRDCHKIG (NAME1, NAME2, IGNORE)
C
C Checks that two database entries have the same crds 
C  Ignoring types in IGNORE
C
C
C	NAME1	CH*(*)	input	Name of Image
C	NAME2	CH*(*)	input	Name of another Image
C       IGNORE  CH*(*)  input   LIST of CRD TYPES to ignore
C
C Audit trail:
C	Un-ashamedly cloned from CRDCHECK                   
C				R.G. Marson     Nov 1 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME1, NAME2
      CHARACTER*(*)     IGNORE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDCHKIG')
C
      INTEGER		STRLEN
      LOGICAL           STRMATCH
C
      CHARACTER*8	TYPE1(SYSMXDIM), TYPE2(SYSMXDIM)
      DOUBLE PRECISION	RVAL1(SYSMXDIM), RVAL2(SYSMXDIM)
      REAL		ROTA1(SYSMXDIM), ROTA2(SYSMXDIM)
      REAL		DELT1(SYSMXDIM), DELT2(SYSMXDIM)
      REAL		RPIX1(SYSMXDIM), RPIX2(SYSMXDIM)
      INTEGER		NAXIS1(SYSMXDIM), NAXIS2(SYSMXDIM)
      INTEGER		NAX1,NAX2
      INTEGER		I, N
      LOGICAL		OK
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
         IF (.NOT.STRMATCH(IGNORE, TYPE1(I))) THEN
            IF (TYPE1(I)  .NE. TYPE2(I) ) OK = .FALSE.
            IF (NAXIS1(I) .NE. NAXIS2(I)) OK = .FALSE.
            IF (ROTA1(I)  .NE. ROTA2(I) ) OK = .FALSE.
            IF (RVAL1(I)  .NE. RVAL2(I) ) OK = .FALSE.
            IF (RPIX1(I)  .NE. RPIX2(I) ) OK = .FALSE.
            IF (DELT1(I)  .NE. DELT2(I) ) OK = .FALSE.
         END IF
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
