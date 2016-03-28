C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftf235.f	1.1    2/10/93
C
      LOGICAL FUNCTION FFTF235(SIZE)
C
CD Returns True if its arguement is a power of 2 and  3 and/or 5
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
C Function Declarations
C
      LOGICAL           FFTF2
C
C Local Variables
C
      INTEGER           ASIZE, I
C
C=======================================================================
C
C If an error on input then exit immediately
C
      FFTF235 = .FALSE.
      IF (ERROR) GO TO 999
C
C Code goes here
C
      ASIZE = SIZE
      DO I = 1, 128
         IF (5*(ASIZE/5).EQ.ASIZE) THEN 
            ASIZE = ASIZE/5
         ELSE 
            GO TO 2
         END IF
      END DO
 2    DO I = 1, 128
         IF (3*(ASIZE/3).EQ.ASIZE) THEN 
            ASIZE = ASIZE/3
         ELSE 
            GO TO 3
         END IF
      END DO
 3    IF (FFTF2(ASIZE).AND.(ASIZE.GE.2)) THEN 
         FFTF235 = .TRUE.
      ELSE
         FFTF235 = .FALSE.
      END IF
C
  999 CONTINUE
      END
