C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxscal.f	1.2    11/7/90
C
       SUBROUTINE PIXXSCAL (IN, SCALE, OFFSET, OUT, N)
C
CD Scale an array
C
C
C	IN	X	input	COMPLEX array
C	SCALE	REAL	input	REAL Scale factor
C	OFFSET	REAL	input	REAL Offset
C	OUT	X	input	COMPLEX array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C	Changed SCALE and OFFSET from COMPLEX to REAL to be consistent
C	with the call from ARRSCALE.  Nothing else calls this routine.
C				M.A.Holdaway	May 18 1990
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      COMPLEX		IN(*), OUT(*)
      REAL		SCALE, OFFSET
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         OUT(I) = SCALE * IN(I) + OFFSET
 10   CONTINUE
C
      CONTINUE
      END
