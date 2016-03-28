C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdgra.f	1.1    1/27/94
C
      DOUBLE PRECISION FUNCTION CRDGRA (NAME)
C
CD Looks up the header info and returns the RA if possible
C
C	NAME	CH*(*)	input	Name of directory entry
C
C Audit trail:
C	Original version: Cloned from CRDGFREQ
C	and successive lines
C				D.S.Briggs	Nov 4 1993
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDGRA')
C
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
C
      INTEGER		I
      DOUBLE PRECISION	RA
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      DO 100 I = 1, NAX
         IF (TYPE(I)(1:2) .EQ. 'RA') THEN
            RA = RVAL(I)
            GOTO 200
         ENDIF
 100  CONTINUE
      RA = 0.0D0
 200  CONTINUE
C
      CRDGRA = RA
C         
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
