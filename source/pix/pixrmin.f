C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrmin.f	1.1    1/31/93
C
      REAL FUNCTION PIXRMIN (A, N)
C
CD Return the minimum of an array
C
C
C	A	REAL	input	Input array
C	N	INT	input	Size of the above array
C Audit trail:
C      Cloned from pixrstat
C                              R. G. Marson 10 Dec 1992
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
C Input Variables
C
      INTEGER           N
      REAL              A(N)
C     
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRMIN')
C
C Function Declarations
C   

C
C Local Variables
C
      INTEGER		I
C=====================================================================
C
C Ignore this routine if an error has occurred
C
      IF (ERROR) GOTO 999
C
C Main loop
C
      PIXRMIN = A(1)
      DO 10 I = 2, N
         PIXRMIN = MIN(A(I), PIXRMIN)
 10   CONTINUE
C
C Clean up
C
 999  CONTINUE
      END
