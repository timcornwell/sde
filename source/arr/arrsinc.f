C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsinc.f	1.1	 5/28/91
C
      SUBROUTINE ARRSINC (NSINC, SADD, RADMAX)
C
CD Make a lookup table for SINC(X)
C
C	NSINC	INT	output	size of SINC array
C	SADD	INT	output	Address of SINC array
C	RADMAX	REAL	output	Maximum value of X in SINC(X)
C
C Audit trail:
C	New routine
C				M.A.Holdaway	May 28 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NSINC,  SADD
      REAL		RADMAX
C
      CHARACTER*(*)	ROUTINE
C
      PARAMETER		(ROUTINE = 'ARRSINC')
C
      CHARACTER*(SYSMXNAM)	NAME
      INTEGER		DATADD
      LOGICAL		DATEXIST
      CHARACTER*6	STRINT
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (RADMAX .EQ. 0.0) THEN
         RADMAX = 10.0
      ENDIF
      NSINC = 16384 * RADMAX/10.0
      NAME = 'SCRATCH/SINC'//STRINT(NINT(RADMAX))
      IF (.NOT.DATEXIST(NAME)) THEN
         IF (.NOT. DATEXIST('SCRATCH')) THEN
            CALL DATCREAT ('SCRATCH')
         ENDIF
         CALL DATMAKAR (NAME, 1, NSINC, 'R', SADD)
         CALL PIXSINC (NSINC, MEMR(SADD), RADMAX )
      ELSE
         SADD = DATADD (NAME)
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
