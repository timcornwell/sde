C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrmax.f	1.1    1/31/93
C
      REAL FUNCTION PIXRMAX (A, N)
C
CD Return the maximum of an array
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
      PARAMETER		(ROUTINE = 'PIXRMAX')
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
      PIXRMAX = A(1)
      DO 10 I = 2, N
         PIXRMAX = MAX(A(I), PIXRMAX)
 10   CONTINUE
C
C Clean up
C
 999  CONTINUE
      END
