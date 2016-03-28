C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixiscpy.f	1.1    10/29/91
C
      SUBROUTINE PIXISCPY (IN, WT, OUT, NIN, NOUT)
C
CD Copy an array conditionally on a weight
C
C	IN	INT	input	Input array
C       WT      REAL    input   Weight array
C	OUT	INT	output	Output array
C	NIN	INT	input	Number of input elements
C	NOUT	INT	in/out  Size of output array on input
C				Number of elements copied on output
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S. Briggs	August 30 1991
C
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER	IN(*), OUT(*)
      REAL	WT(*)
      INTEGER	NIN, NOUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXISCPY')
C
      INTEGER	I, OUTLIM
C=========================================================================
      IF (ERROR) GO TO 999
C
      OUTLIM = NOUT
      NOUT = 0
      DO 10 I = 1, NIN
         IF (WT(I).GT.0.0) THEN
            NOUT = NOUT + 1
            IF (NOUT.GT.OUTLIM) THEN
               CALL ERRREPOR (ERRNOMEM, ROUTINE,
     $            'Size of output array exceeded')
               GO TO 999
            END IF
            OUT(NOUT) = IN(I)
         END IF
  10  CONTINUE
C
 999  CONTINUE
      END
