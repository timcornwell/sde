C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftpwr2.f	1.3    11/7/90
C
      INTEGER FUNCTION FFTPWR2 (I)
C
CD Finds the next power of two.
C
C	I	INT	input	Number
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C----------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		I
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTPWR2')
C
      INTEGER		LOG2
C=========================================================================
      IF (ERROR) GO TO 999
C
      IF (I.LT.1) THEN
         FFTPWR2 = 1
         GO TO 999
      END IF
      LOG2 = NINT(LOG(FLOAT(I))/LOG(2.0))
      FFTPWR2 = 2**LOG2
    1 IF (FFTPWR2.LT.I) THEN
         FFTPWR2 = 2*FFTPWR2
         GO TO 1
      END IF
C
 999  CONTINUE
      END
