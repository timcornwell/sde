C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrphase.f	1.3    11/7/90
C
      SUBROUTINE ARRPHASE (A, SHIFT)
C
CD Phase rotation of an array
C
C
C	A	CH*(*)	input	Name of array
C	SHIFT	REAL(*)	input	Shift in fraction of the field
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Don't call PIXPHASE if shift is zero
C				T.J.Cornwell	April 24 1990
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A
      REAL		SHIFT(*)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRPHASE')
C
      CHARACTER*1	T
      REAL		RPIX(SYSMXDIM)
      INTEGER		I, N, NAXIS(SYSMXDIM)
      INTEGER		AADD, NDUMMY
      LOGICAL		DOSHIFT
      DATA		NAXIS /SYSMXDIM*1 /
      DATA		RPIX /SYSMXDIM*1.0/
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, N, NAXIS, T, AADD)
      DOSHIFT = .FALSE.
      DO 10 I = 1, N
         DOSHIFT = DOSHIFT.OR.(SHIFT(I).NE.0.0)
         NAXIS(I) = MAX (1, NAXIS(I))
  10  CONTINUE
      CALL DATGETR (A, 'CRPIX', RPIX, SYSMXDIM, NDUMMY)
      IF(.NOT.DOSHIFT) GO TO 999
C
C Call appropriate routine
C
      IF (T.EQ.'X') THEN
         CALL PIXPHASE (MEMX(AADD), SHIFT, RPIX,
     1      NAXIS(1), NAXIS(2), NAXIS(3), NAXIS(4), NAXIS(5), 
     2      NAXIS(6), NAXIS(7))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1     'Array type not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
