C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdcopy.f	1.2	 2/19/91
C
      SUBROUTINE CRDCOPY (NAME1, NAME2)
C
CD Copies the coordinate info from NAME1 to NAME2:  
C Checks that the CRD info and the ARRAY info match
C
C Arguments: CALL CRDCOPY (NAME1, NAME2)
C
C	NAME1	CH*(*)	input	Name of Image
C	NAME2	CH*(*)	input	Name of another Image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 14 1990
C	Removed illegal concatenation of strings
C				T.J. Cornwell   Feb 19 1991
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME1, NAME2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDCOPY')
C
      LOGICAL		OK
      INTEGER		I, N, IADD
      INTEGER		NAX1,NAX2, NAXM
      INTEGER		NAXIS1(SYSMXDIM), NAXIS2(SYSMXDIM)
      CHARACTER		T
      CHARACTER*8	TYPE1(SYSMXNAM), TYPE2(SYSMXNAM)
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
      CALL DATGETAR (NAME2, NAX2, NAXIS2, T, IADD)
      NAXM = MAX(NAX1, NAX2)
      DO 100 I = 1, NAXM
         IF (NAXIS1(I) .GT. 1 .OR. NAXIS2(I) .GT. 1) THEN
            IF (NAXIS1(I) .NE. NAXIS2(I))  OK = .FALSE.
         ENDIF
 100  CONTINUE
      IF (OK) THEN
         CALL CRDPUT (NAME2, NAX1, TYPE1, NAXIS1, RVAL1, RPIX1, DELT1,
     1      ROTA1)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, ' arrays are incompatible')
         CALL CRDLIST (NAME1)
         CALL CRDLIST (NAME2)               
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
