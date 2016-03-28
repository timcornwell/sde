C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixcsetc.f	1.1	 12/10/92
C
      SUBROUTINE PIXCSETC (A, CONST, N)
C
CD Initialize an array
C
C
C	A	CH*(*)	input	Array to be set
C	CONST	CH*1	input	Constant
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)		A(*)
      CHARACTER*1	CONST
      INTEGER		N
C
C
      INTEGER		I
C=====================================================================
      DO 10 I = 1, N
         A(I) = CONST
 10   CONTINUE
C
      CONTINUE
      END
