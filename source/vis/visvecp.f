C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visvecp.f	1.2 11/18/94
C
      SUBROUTINE VISVECP (VIS, NVIS, WT, VECTOR, NROWS)
C
CD Create Visibility Vector
C
C
C	VIS	CMPLX	input	Visibility function
C	NVIS	INT	input	Number of visibilities
C	WT	REAL	input	Weights
C	U, V, W	REAL	input	spatial frequencies in waves.
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Nov 16 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NROWS, NVIS
      REAL		WT(*)
      COMPLEX		VIS(*)
      REAL		VECTOR(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISVECP')
C
      INTEGER		IVIS, IROW
      REAL		RNORM
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 1 IROW = 1, NROWS
         VECTOR(IROW) = 0.0
 1    CONTINUE
C
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).GT.0.0) THEN
            IROW = 2 * (IVIS-1) + 1
            VECTOR (IROW)          =  WT(IVIS) * REAL (VIS(IVIS))
            VECTOR (IROW+1)        =  WT(IVIS) * AIMAG(VIS(IVIS))
         END IF
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
