C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixpc2x.f	1.2	 3/29/91
C
      SUBROUTINE PIXPC2X (N, CARR, AARR, PARR)
C
CD Polarized amplitude and polarization position angle to X
C
C CALL PIXPC2X (N, CARR, AARR, PARR)
C
C	N	INT	input	total number of pixels
C	CARR	X	input	Complex Array
C	AARR	R	input	AMP of CARR
C	PARR	R	input	Phase of  CARR
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Feb 5 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		N
      COMPLEX		CARR (*)
      REAL		AARR (*), PARR(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXPC2X')
      INTEGER		I
      REAL		AMP, PI, DTOR2
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      PI = ATAN2 (1.0, 1.0) * 4.0
      DTOR2 = 2.0 * PI / 180.0
C
      DO 10 I = 1, N
         CARR(I) = AARR(I) * CMPLX( COS(DTOR2*PARR(I)), 
     $      SIN(DTOR2*PARR(I)))
 10   CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
