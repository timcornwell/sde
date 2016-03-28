C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdscal.f	1.3    11/7/90
C
       SUBROUTINE PIXDSCAL (IN, SCALE, OFFSET, OUT, N)
C
CD Scale an array
C
C
C	IN	DBLE	input	Real array
C	SCALE	REAL	input	Scale factor
C	OFFSET	REAL	input	Offset
C	OUT	DBLE	input	Real array
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      DOUBLE PRECISION	IN(*), OUT(*)
      REAL              SCALE, OFFSET
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
