C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visshifp.f	1.2    11/7/90
C
      SUBROUTINE VISSHIFP (VIS, WT, NVIS, U, V, W, SHIFT,
     1    NEWVIS)
C
CD Shift visibility data.
C
C
C	VIS	CMPLX	input	Input visibility
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	U,V,W	REAL	input	U,V,W in wavelengths
C	SHIFT	REAL	input	Shift in arcseconds
C	NEWVIS	REAL	output	Output visibilities
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS
      COMPLEX		VIS(NVIS), NEWVIS(NVIS)
      REAL		WT(NVIS), U(NVIS), V(NVIS), W(NVIS)
      REAL		SHIFT(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSHIFP')
C
      INTEGER		IVIS
      REAL		TWOPI, PHASE, L, M, N, STOR
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      TWOPI = 8 * ATAN(1.0)
      STOR = TWOPI / (360.0 * 3600.0)
C
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         PHASE = TWOPI * STOR * (U(IVIS) * SHIFT(1) +
     1       V(IVIS) * SHIFT(2) + W(IVIS) * SHIFT(3))
         NEWVIS(IVIS) = VIS(IVIS) * CMPLX(COS(PHASE), -SIN(PHASE))
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
