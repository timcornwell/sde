C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrrsym.f	1.1	 2/21/91
C
      SUBROUTINE ARRRSYM (IN, SYM, ANTI)
C
CD Breaks a real array into symmetric and antisymmetric parts
C
C Arguments: CALL ARRRSYM (IN, SYM, ANTI)
C
C	IN	CH*(*)	input	Name of INPUT Real Array
C	SYM	CH*(*)	input	Name of symetric part of image, out
C	ANTI	CH*(*)	input	Name of antisym part of image, out
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 14 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IN, SYM, ANTI
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRRSYM')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      INTEGER		IADD, SADD, AADD
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER		ITYPE, STYPE, ATYPE
      CHARACTER*(SYSMXNAM)	NULL
      LOGICAL		DATEXIST
      DATA	NULL /'NULL'/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR  (IN, NAX, NAXIS, ITYPE, IADD)
      IF (ITYPE .NE. 'R') THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Type must be R, not '//ITYPE )
            GOTO 999
         ENDIF
      IF (.NOT.DATEXIST (SYM)) THEN
         CALL DATMAKAR  (SYM, NAX, NAXIS, ITYPE, SADD)
      ELSE
         CALL DATGETAR  (SYM, NAX, NAXIS, STYPE, SADD)
         IF (STYPE .NE. 'R') THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Type must be R, not '//STYPE )
            GOTO 999
         ENDIF
      ENDIF
      IF (.NOT.DATEXIST (ANTI)) THEN
         CALL DATMAKAR  (ANTI, NAX, NAXIS, 'R', AADD)
      ELSE
         CALL DATGETAR  (ANTI, NAX, NAXIS, ATYPE, AADD)
         IF (ATYPE .NE. 'R') THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Type must be R, not '//ATYPE )
            GOTO 999
         ENDIF
      ENDIF
      CALL CRDCHECK (IN, SYM)
      CALL CRDCHECK (IN, ANTI)
      IF (ERROR) GOTO 990
      CALL ARRSETCO (SYM, 0., 0.)
      CALL ARRSETCO (ANTI, 0., 0.)
C      
      IF (NAXIS(2) .LE. 1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'No 1-D arrays, please')
         GOTO 999
      ELSE IF (NAXIS(3) .GT. 1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Only 2-D arrays, please')
         GOTO 999
      ENDIF
C
      CALL CRDGET (IN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     1   ROTA)
      CALL PIXRSYM (NAXIS(1), NAXIS(2), RPIX(1), RPIX(2), 
     $   MEMR(IADD), MEMR(SADD), MEMR(AADD))
      IF (ERROR) GOTO 999
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
