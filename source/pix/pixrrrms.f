C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrrrms.f	1.1 9/29/92
C
      SUBROUTINE PIXRRRMS (IN, N, SIGMA, RMS)
C
CD Form robust rms
C
C
C	IN	REAL	input	Input array
C	N	INT	input	Number of elements
C Audit trail:
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	IN(*), SIGMA, RMS
      INTEGER	N
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRRRMS')
C
      INTEGER	I, IPASS, AN
      REAL	NRMS, ORMS, EPS
      DATA	EPS/0.01/
C=========================================================================
      IF (ERROR) GO TO 999
C
      NRMS=0.0
      DO 5 I = 1, N
         NRMS=NRMS+IN(I)**2
 5    CONTINUE
      NRMS=SQRT(NRMS/N)
      IF(NRMS.EQ.0.0) GO TO 999
C
      DO 20 IPASS=1,10
         ORMS=NRMS
         NRMS=0.0
         AN=0
         DO 10 I = 1, N
            IF(ABS(IN(I)).LT.SIGMA*ORMS) THEN
               NRMS=NRMS+IN(I)**2
               AN=AN+1
            END IF
  10     CONTINUE
         IF(AN.GT.0) THEN
            NRMS=SQRT(NRMS/AN)
         ELSE
            NRMS=0.0
         ENDIF
         IF(SYSDEBUG) THEN
            WRITE(MESSAGE, 1000) ORMS, NRMS
 1000       FORMAT ('Old, new rms = ',1PE12.3,', ',1PE12.3)
            CALL MSGPUT (MESSAGE, 'I')
         END IF
         IF(ABS(NRMS-ORMS).LT.EPS*ORMS) GO TO 999
 20   CONTINUE
C
 999  CONTINUE
      RMS=NRMS
      END
