C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrbsch.f	1.3    11/7/90
C
      SUBROUTINE PIXRBSCH (ARRAY, SIZE, VALUE, POSITION)
CD     
C    Search array for position at which value occurs using
C    a binary search. This assumes the array is sorted in increasing
C    order
C    
C    position   INTEGER output Position at which value first occurs  
C    size       INTEGER input  size of array
C    array      REAL   input	data array to be searched
C    value      REAL   input   value to search for
C
C    Audit trail:
C    Cloned from pixrindx
C                                    R.G. Marson     Feb 15 1990
C    
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER           SIZE, POSITION
      REAL		ARRAY(SIZE), VALUE
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRBSCH')
C     
      INTEGER		I,WSIZE, NSTEPS
C=====================================================================
C     
C     If an error on input then exit immediately
C     
      IF (ERROR) GO TO 999
C
C Find next power of two greater than size.
C
      NSTEPS = INT( LOG(FLOAT(SIZE)) / LOG(2.0) )
      WSIZE = 2 ** NSTEPS
C
C Begin the binary search
C
      POSITION = WSIZE
      DO I = NSTEPS, 1, -1
         WSIZE = WSIZE/2
         IF (POSITION.LE.SIZE) THEN
            IF (ARRAY(POSITION).GE.VALUE) THEN
               POSITION = POSITION - WSIZE
            ELSE
               POSITION = POSITION + WSIZE
            END IF
         ELSE
            POSITION = POSITION - WSIZE
         END IF
      END DO
C
C Check for errors and clean up
C
      IF (POSITION.GT.SIZE) THEN 
         POSITION = -1
      ELSE IF (ARRAY(POSITION).LT.VALUE) THEN
         POSITION = POSITION + 1
      END IF
      IF (POSITION.GT.SIZE) POSITION = -1
C
C     Can jump to here if an error found
C     
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C     
 999  CONTINUE
      END





