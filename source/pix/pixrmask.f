C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrmask.f	1.1    12/20/90
C
      SUBROUTINE PIXRMASK (IN, OUT, N, AMIN, AMAX, FLUX1, FLUX2,FLUX3)
C
CD For Pixels with flux between MIN and MAX, give flux of FLUX1
C
C
C	IN	REAL	input	Input array
C	OUT	REAL	output	Output array
C	N	INT	input	Number of elements
C	AMIN	R	in	Min value
C	AMAX	R	in	Max value
C	FLUX1	R	in	Assignment Value: Too Low
C	FLUX2	R	in	Assignment Value: In Range
C	FLUX3	R	in	Assignment Value: Too high
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Sept 16 1990
C
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	IN(*), OUT(*), AMIN, AMAX, FLUX1, FLUX2, FLUX3
      INTEGER	N
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRMASK')
C
      INTEGER	I
C=========================================================================
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
         IF (IN(I) .LT. AMIN) THEN
            OUT(I) = FLUX1
         ELSE IF (IN(I) .GT. AMAX) THEN
            OUT(I) = FLUX3
         ELSE
            OUT(I) = FLUX2
         ENDIF
  10  CONTINUE
C
 999  CONTINUE
      END
