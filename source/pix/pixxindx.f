C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxindx.f	1.2    11/7/90
C
      SUBROUTINE PIXXINDX (INDEX, SIZE, DATA, SCRATCH)
CD     
C    Re-order the data using index as an index (Complex arrays)
C    
C    index      INTEGER input  index array
C                              (contains values from 1 to size)
C    size       INTEGER input  size of all these arrays
C    data       COMPLEX input  data array to be reordered (in place)
C    scratch    COMPLEX input  scratch array
C
C    Audit trail:
C    Cloned from pixdindx
C                                    R.G. Marson     Feb 13 1990
C    
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER           SIZE, INDEX(SIZE)
      COMPLEX           DATA(SIZE), SCRATCH(SIZE)
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXXINDX')
C     
      INTEGER		I
C=====================================================================
C     
C     If an error on input then exit immediately
C     
      IF (ERROR) GO TO 999
C
C Copy data array to scratch
C     
      DO 100, I=1, SIZE
         SCRATCH(I) = DATA(I)
 100  CONTINUE
C
C Reorder scratch back to data
C
      DO 200, I=1,SIZE
         DATA(I) = SCRATCH(INDEX(I))
200   CONTINUE
C     
C     Can jump to here if an error found
C     
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C     
 999  CONTINUE
      END
