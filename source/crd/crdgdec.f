C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdgdec.f	1.1    1/27/94
C
      DOUBLE PRECISION FUNCTION CRDGDEC (NAME)
C
CD Looks up the header info and returns the Dec if possible
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
      PARAMETER		(ROUTINE = 'CRDGDEC')
C
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
C
      INTEGER		I
      DOUBLE PRECISION	DEC
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      DO 100 I = 1, NAX
         IF (TYPE(I)(1:3) .EQ. 'DEC') THEN
            DEC = RVAL(I)
            GOTO 200
         ENDIF
 100  CONTINUE
      DEC = 0.0D0
 200  CONTINUE
C
      CRDGDEC = DEC
C         
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
