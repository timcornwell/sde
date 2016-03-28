C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdexist.f	1.1    7/11/93
C
      LOGICAL FUNCTION CRDEXIST (NAME)
C
C Returns true if NAME has a legal coordinate system
C
C	NAME	CH*(*)	input	Name of directory entry
C
C The check isn't bullet proof, but it should be plenty good enough
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	27 June 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDEXIST')
C
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C
      CRDEXIST = .FALSE.
C
      IF (.NOT.DATEXIST(NAME)) GO TO 999
      IF (.NOT.DATEXIST(STRM2(NAME, 'CTYPE'))) GO TO 999
      IF (.NOT.DATEXIST(STRM2(NAME, 'CRVAL'))) GO TO 999
      IF (.NOT.DATEXIST(STRM2(NAME, 'CRPIX'))) GO TO 999
      IF (.NOT.DATEXIST(STRM2(NAME, 'CDELT'))) GO TO 999
      IF (.NOT.DATEXIST(STRM2(NAME, 'CROTA'))) GO TO 999
C
      CRDEXIST = .TRUE.
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
