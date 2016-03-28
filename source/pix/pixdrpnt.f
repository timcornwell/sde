C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdrpnt.f	1.1    12/26/91
C
      SUBROUTINE PIXDRPNT (A, NX, NY, PX, PY, VAL)
C
CD Draw a (point) pixel into an array
C
C	A	R(NX,NY) output Array (frame buffer)
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	PX	REAL	input	X position of point, in pixels
C	PY	REAl	input	Y position of point, in pixels
C	VAL	REAL	input	Cursor value to draw into array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Nov 23 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY
      REAL		A(NX,NY)
      REAL		PX, PY, VAL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXDRPNT')
C
      INTEGER		IX, IY
C
      INTEGER		NINT
C=====================================================================
      IF (ERROR) GO TO 999
C
      IX = NINT(PX)
      IY = NINT(PY)
      IF ((IX.GE.1).AND.(IX.LE.NX).AND.(IY.GE.1).AND.(IY.LE.NY))
     $   A(IX,IY) = VAL
C
 999  CONTINUE
      END

