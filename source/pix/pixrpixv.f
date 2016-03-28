C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrpixv.f	1.3    11/7/90
C
      SUBROUTINE PIXRPIXV (A,V,N)
C
CD PIXVAL 
C
C
C	A	REAL	input	Real array
C      V       REAL    output  Value returned for central pixel
C	N	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'PIXBAND4')
C
      REAL		A(*),V
      INTEGER		N,I
C
C=====================================================================
      I = N/4
      V = A(I)
C
 999  CONTINUE
      END
