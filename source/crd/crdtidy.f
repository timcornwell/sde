C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdtidy.f	1.1    6/13/94
C
      SUBROUTINE CRDTIDY (NAME, TNAX)
C
C Ensure that a coordinate system is acceptable to CRDPTOW and friends.
C
C	NAME	CH*(*)	input	Name of directory entry
C	TNAX	INT	input	Tidy at least this many axes
C
C This routine will make sure that at least MAX(TNAX,NAX) coordinate
C axes in the header have valid entries, creating null entries or
C overriding DELT if required
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	June 10 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
      INTEGER		TNAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDTIDY')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), 
     1   		DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
C
      INTEGER		IAX
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GO TO 999
C
      DO 100 IAX = 1, MAX(NAX,TNAX)
         IF (IAX.GT.NAX) THEN
            TYPE(IAX) = 'NULL'
            RVAL(IAX) = 0.0D0
            RPIX(IAX) = (NAXIS(IAX)+1)/2
            DELT(IAX) = 1.0
            ROTA(IAX) = 0.0
         ELSE
            IF (DELT(IAX).EQ.0.0) THEN
               IF (RVAL(IAX).EQ.0.0D0) THEN
                  DELT(IAX) = 1.0
               ELSE
                  DELT(IAX) = 0.1 * RVAL(IAX)
               END IF
            END IF
         END IF
 100  CONTINUE
C
      CALL CRDPUT (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
