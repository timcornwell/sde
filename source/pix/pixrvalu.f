C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrvalu.f	1.1    12/7/90
C
      REAL FUNCTION PIXRVALU (ARRAY, I, NAXIS)
C
CD This function returns the value of real ARRAY(I1, I2, I3, I4, I5, I6, I7)
C
C	ARRAY	REAL	input	A REAL array 
C       NAXIS   INTEGER input   The size of the above array
C       I1..I7  INTEGER input   The indices of this array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Nov 25 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input declarations
C
      INTEGER           I(SYSMXDIM), NAXIS(SYSMXDIM)
      REAL              ARRAY(NAXIS(1), NAXIS(2), NAXIS(3), 
     $                        NAXIS(4), NAXIS(5), NAXIS(6), NAXIS(7))
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRVALU')
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Code goes here
C
      PIXRVALU = ARRAY(I(1), I(2), I(3), I(4), I(5), I(6), I(7))
C
  999 CONTINUE
      END
