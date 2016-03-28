C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrexpan.f	1.1 8/14/92
C
      SUBROUTINE ARREXPAN (A, NAX, NAXIS)
C
CD Expand an array, returning the same name and with info intact
C
C
C	A	CH*(*)	input	Directory name of array
C	NAX	INT	input	New number of axes
C	NAXIS	INT(*)	input	Number of pixels on each axis
C Audit trail:
C	TRC was previously being compared to ANAXIS rather than BNAXIS
C				T.J.Cornwell	Feb 24 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	A
      INTEGER		NAX, NAXIS(*)
C
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARREXPAN')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE
      CHARACTER*(SYSMXNAM)		B
      INTEGER 		ANAX, ANAXIS(SYSMXDIM)
      INTEGER 		BNAX, BNAXIS(SYSMXDIM)
      INTEGER 		IAX, NDUMMY, STRLEN
      DATA 		ANAXIS	/SYSMXDIM*1/
      DATA 		BNAXIS	/SYSMXDIM*1/
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
      B = A(1:STRLEN(A))//'TEMP'
C
C Get array attributes
C
      CALL DATGETAR (A, ANAX, ANAXIS, ATYPE, NDUMMY)
      IF (ERROR) GO TO 990
C
C Figure out size of new array
C
      BNAX = MIN(MAX(NAX, ANAX), SYSMXDIM)
      DO 25 IAX = 1, BNAX
         BNAXIS(IAX) = MAX(ANAXIS(IAX), NAXIS(IAX))
  25  CONTINUE
      CALL DATMAKAR (B, BNAX, BNAXIS, ATYPE, NDUMMY)
C
C Insert into new array. Delete old and rename new to old
C
      CALL ARRSETCO (B, 0.0, 0.0)
      CALL ARRINSER (A, B, ' ')
      CALL HEDCOPY (A, B)
      CALL DATDELET (A)
      CALL DATRENAM (B, A)
C
C Trace errors
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
