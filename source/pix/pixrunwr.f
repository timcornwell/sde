C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrunwr.f	1.1	 14 Aug 1995
C
      SUBROUTINE PIXRUNWR (PH, N)
C
CD Given a vector representing phase in degrees, I unwrap the phase
C
C	PH	REAL	in/out	Phase array
C	N	INT	input	Size of vector
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	28 Nov 1994
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		PH(*)
      INTEGER		N
C
      INTEGER		I
      REAL		PMEAN
C=====================================================================
      IF (ERROR) GOTO 990
      DO 450 I = 2, N
         IF (ABS(PH(I)  - PH(I-1)) .GT.  180. ) THEN
            PH(I) = PH(I) + 360.0 * NINT ( (PH(I-1)  - PH(I))/360.0)
         ENDIF
 450  CONTINUE
C
 990  CONTINUE
      END
