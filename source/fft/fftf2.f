C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftf2.f	1.1    2/10/93
C
      LOGICAL FUNCTION FFTF2(SIZE)
C
CD Returns True if its arguement is a power of 2
C
C	SIZE	INTEGER	input arguement
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Oct 9 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input parameters
C
      INTEGER           SIZE
C
C Local Variables
C
      INTEGER           ASIZE
C
C=======================================================================
C
C If an error on input then exit immediately
C
      FFTF2 = .FALSE.
      IF (ERROR) GO TO 999
C
C Code goes here
C
      IF (SIZE.NE.0) THEN
         ASIZE = ABS(SIZE)
         FFTF2 = (ASIZE.EQ.2**NINT(LOG(FLOAT(ASIZE))/LOG(2.0)))
      ELSE
         FFTF2 = .TRUE.
      END IF
C
  999 CONTINUE
      END
