C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdlisrd.f	1.1	 6/5/93
C
      SUBROUTINE CRDLISRD (NAME)
C
CD List RA & Dec of image if found
C
C	NAME	CH*(*)	input	Name of directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C					D.S.Briggs	May 12 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDLISRD')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), 
     1   		DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
C
      REAL		RRA(3), RDEC(3)
      CHARACTER*15	ARA, ADEC
      INTEGER		I, IRA, IDEC
C
      INTEGER		STRLEN
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     1   ROTA)
C
      IRA = -1
      IDEC = -1
      DO 100 I = 1, NAX
         IF (TYPE(I)(1:2).EQ.'RA') IRA = I
         IF (TYPE(I)(1:3).EQ.'DEC') IDEC = I
 100  CONTINUE
C
      IF ((IRA.GT.0).AND.(IDEC.GT.0)) THEN
         CALL CRDD2RD (RVAL(IRA), RVAL(IDEC), RRA, RDEC, ARA, ADEC)
         WRITE (MESSAGE, 1000) ARA(1:STRLEN(ARA)), ADEC(1:STRLEN(ADEC))
 1000    FORMAT ('Reference RA = ',A,'  Dec = ',A)
         CALL MSGPUT (MESSAGE,'I')
      ELSE
         CALL MSGPUT ('Can not find RA & Dec axes!','W')
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
