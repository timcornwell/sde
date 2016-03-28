C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdgfreq.f	1.1    7/19/91
C
      REAL FUNCTION CRDGFREQ (NAME)
C
CD Looks up the frequency header info and returns the frequency if
C  possible
C
C	NAME	CH*(*)	input	Name of directory entry
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway 	July 9 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDGFREQ')
C
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
C
      INTEGER		I
      REAL		FREQ
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      DO 100 I = 1, NAX
         IF (TYPE(I) .EQ. 'FREQ') THEN
            FREQ = RVAL(I)
            GOTO 200
         ENDIF
 100  CONTINUE
      FREQ = 0.0
 200  CONTINUE
C
      CRDGFREQ = FREQ
C         
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
