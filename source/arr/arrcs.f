C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrcs.f	1.3    11/7/90
C
      SUBROUTINE ARRCS (NCS, CSADD, PSCL, COFFSET)
C
CD Make a lookup table for the cosine/sine function
C
C
C	NCS	output	INT	Size of look up table
C	CSADD	output	INT	Address of lookup table
C	PSCL	output	REAL	scaling number
C	COFFSET	output	INT	Offset to get cosines
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NCS, CSADD, COFFSET
      REAL		PSCL
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRCS')
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      INTEGER		DATADD, ICS
      LOGICAL		DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NCS = 65536
      PSCL = FLOAT(NCS) / (2.5*PI)
      COFFSET = NINT(PSCL * PI / 2.0)
      IF (.NOT.DATEXIST('SCRATCH/CS')) THEN
         CALL DATCREAT ('SCRATCH')
         CALL DATMAKAR ('SCRATCH/CS', 1, 2 * NCS + 1, 'R', CSADD)
         DO 5 ICS = 1, 2 * NCS + 1
            MEMR(CSADD+ICS-1) = SIN(FLOAT(ICS-NCS-1)/PSCL)
  5      CONTINUE
      ELSE
         CSADD = DATADD ('SCRATCH/CS')
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
