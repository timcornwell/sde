C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdnhalf.f	1.3    11/7/90
C
      SUBROUTINE CRDNHALF (NAME, BLC, TRC)
C
CD Adjust BLC, TRC to be no more than half an axis minus 1 pixel
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	BLC,TRC	INT(*)	i/o	BLC, TRC on each axis
C Audit trail:
C	New routine
C				T.J. Cornwell	August 23 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	NAME
      INTEGER		BLC(*), TRC(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDNHALF')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
C
      INTEGER		IAX, IBEG, IEND, IDEL, ADD
      CHARACTER*(1)	ATYPE
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, ADD)
C
      DO 10 IAX = 1, SYSMXDIM
C
         IBEG = MAX(1, MIN(BLC(IAX), NAXIS(IAX)))
         IEND = MAX(1, MIN(TRC(IAX), NAXIS(IAX)))
         BLC(IAX) = MIN (IBEG, IEND)
         TRC(IAX) = MAX (IBEG, IEND)
         IF(NAXIS(IAX).GT.1) THEN
            IF(BLC(IAX).EQ.1) THEN
               BLC(IAX) = NAXIS(IAX) / 4 + 1
            END IF
            IF(TRC(IAX).EQ.1) THEN
               TRC(IAX) = 3 * NAXIS(IAX) / 4 - 1
            END IF
            IDEL = MIN(TRC(IAX)-BLC(IAX)+1,NAXIS(IAX)/2-1)
            TRC(IAX) = BLC(IAX) + IDEL - 1
         END IF
C
  10  CONTINUE
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
