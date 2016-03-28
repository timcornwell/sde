C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdnull.f	1.1    7/11/93
C
      SUBROUTINE CRDNULL (NAME)
C
C Insert a null coordinate system into an array
C
C	NAME	CH*(*)	input	Name of directory entry
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
      PARAMETER		(ROUTINE = 'CRDNULL')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), 
     1   		DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      CHARACTER*1	ATYPE
C
      INTEGER		NDUMMY, IAX
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, NDUMMY)
      IF (ERROR) GO TO 999
C
      DO 10 IAX = 1, SYSMXDIM
         TYPE(IAX) = 'NULL'
         RVAL(IAX) = 0.0D0
         RPIX(IAX) = (NAXIS(IAX)+1)/2
         DELT(IAX) = 1.0
         ROTA(IAX) = 0.0
 10   CONTINUE
C
      CALL CRDPUT (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
