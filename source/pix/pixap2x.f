C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixap2x.f	1.2	 3/29/91
C
      SUBROUTINE PIXAP2X (N, CARR, AARR, PARR)
C
CD Converts a AMP and PHASE arrays into a COMPLEX array
C
C CALL PIXAP2X (N, CARR, AARR, PARR)
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
C	Switch from SIND to SIN
C				M.A.Holdaway	March 29 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		N
      COMPLEX		CARR (*)
      REAL		AARR (*), PARR(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXAP2X')
      INTEGER		I
      REAL		AMP, PI, DTOR
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      PI = ATAN2 (1.0, 1.0) * 4.0
      DTOR = PI / 180.0
C
      DO 10 I = 1, N
         CARR(I) = AARR(I) * CMPLX( COS(DTOR*PARR(I)), 
     $      SIN(DTOR*PARR(I)))
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
