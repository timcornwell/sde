C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrap2x.f	1.1	 2/14/91
C
      SUBROUTINE ARRAP2X (REAL1, REAL2, COMP)
C
CD Makes a complex array out of two real arrays: COMP = REAL1 * EXP( i REAL2)
C
C Arguments: CALL ARRAP2X (REAL1, REAL2, COMP)
C
C	REAL1	CH*(*)	input	Name of AMP
C	REAL2	CH*(*)	input	Name of PHASE
C	COMP	CH*(*)	input	Name out OUTPUT Complex Image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 14 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	REAL1, REAL2, COMP
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRAP2X')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), R1ADD, R2ADD, I, NT
      INTEGER		XADD
      CHARACTER		ATYPE
      CHARACTER*(SYSMXNAM)	NULL
      LOGICAL		DATEXIST
      DATA	NULL /'NULL'/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (REAL1 .EQ. NULL) THEN
         CALL IMGCLONE  (REAL2, REAL1)
         CALL ARRSETCO  (REAL1, 0., 0.)
      ELSE IF (REAL2 .EQ. NULL) THEN
         CALL IMGCLONE  (REAL1, REAL2)
         CALL ARRSETCO  (REAL2, 0., 0.)
      ENDIF
      IF (ERROR) GOTO 999
      CALL DATGETAR  (REAL2, NAX, NAXIS, ATYPE, R2ADD)
      CALL DATGETAR  (REAL1, NAX, NAXIS, ATYPE, R1ADD)
      IF (ATYPE .NE. 'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Code only for REAL arrays')
         GOTO 990
      ENDIF
      CALL CRDCHECK (REAL1, REAL2)
      IF (ERROR) GOTO 999
      NT = 1
      DO 100 I = 1, NAX
         NT = NT * NAXIS(I)
 100  CONTINUE
C
      IF (.NOT. DATEXIST(COMP) ) THEN
         CALL DATMAKAR (COMP, NAX, NAXIS, 'X', XADD)
      ELSE
         CALL CRDCHECK (REAL2, COMP)
         CALL DATGETAR (COMP, NAX, NAXIS, ATYPE, XADD)
      ENDIF
C         
      CALL PIXAP2X (NT, MEMX(XADD), MEMR(R1ADD), MEMR(R2ADD) )
C
      IF (ERROR) GOTO 999
      IF (DATEXIST(NULL)) CALL DATDELET (NULL)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
