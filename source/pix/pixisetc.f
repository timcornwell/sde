C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixisetc.f	1.3	 6/27/91
C
      SUBROUTINE PIXISETC (A, CONST, N)
C
CD Initialize an array
C
C
C	A	REAL	input	Array to be set
C	CONST	REAL	input	Constant
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 27 1991
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		A(*)
      REAL		CONST
      INTEGER		N
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         A(I) = NINT ( CONST )
 10   CONTINUE
C
      CONTINUE
      END
