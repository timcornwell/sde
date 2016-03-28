C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrflim.f	1.3    11/7/90
C
      SUBROUTINE ARRFLIM (A, NL, FLIM)
C
CD Find a limit such that at most NL points are greater than it in
C absolute value.
C
C
C	A	CH*(*)	input	Name of array: must point to a 
C				histogram
C	NL	INT	input	Maximum number allowed
C	FLIM	REAL	output	Cutoff
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A
      INTEGER		NL
      REAL		FLIM
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRFLIM')
C
      CHARACTER*1	TA
C
      REAL		BASE, INCR
      INTEGER		I, NA, NB, SLIM, ANAXIS(SYSMXDIM)
      INTEGER		ADDA, NDUMMY
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, NA, ANAXIS, TA, ADDA)
      NB = ANAXIS(1)
C
C Call appropriate routine
C
      CALL PIXFLIM (MEMI(ADDA), NB, NL, SLIM)
C
C Convert from index to real value
C
      CALL DATGETR (A, 'BASE', BASE, 1, NDUMMY)
      CALL DATGETR (A, 'INCREMENT', INCR, 1, NDUMMY)
      FLIM = INCR * (SLIM - 1) + BASE + 0.5 * INCR
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
