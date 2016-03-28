C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdrpcen.f	1.3	 7/20/92
C
      SUBROUTINE CRDRPCEN (IN)
C
CD Will shift the reference pixel to NX/2 (or 1, if gridded vis), NY/2 
C
C	IN	CH*(*)	input	Name of directory entry
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 20 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDRPCEN')
C
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), ROTA(SYSMXDIM), DELT(SYSMXDIM)
      INTEGER		NAXIS(SYSMXDIM), NAX
      CHARACTER*8	TYPE(SYSMXDIM)
      INTEGER		I
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C Is this, by any chance, a gridded visibility?
C
      IF (INDEX (TYPE(1), 'UU') .NE. 0 .AND. NAXIS(1) .EQ. 
     $   (NAXIS(2)/2 + 1) ) THEN
         RPIX(1) = 1.0
      ELSE
         RPIX(1) = NAXIS(1)/2.0
      ENDIF
C      
      DO 100 I = 2, NAX
         RPIX(I) = MAX ( NAXIS(I)/2.0, 1.0)
 100  CONTINUE
      CALL CRDPUT (IN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
