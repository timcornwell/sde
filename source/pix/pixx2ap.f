C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixx2ap.f	1.3	 1/31/93
C
      SUBROUTINE PIXX2AP (N, CARR, AARR, PARR)
C
CD Converts a 2D complex array into AMP and PHASE
C
C CALL PIXX2AP (N, CARR, AARR, PARR)
CCS	N	INT	input	total number of pixels
C	CARR	X	input	Complex Array
C	AARR	R	input	AMP of CARR
C	PARR	R	input	Phase of  CARR
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Feb 5 1990
C	Replaced ATAN2D call with equivalent ATAN2 call
C				D.S.Briggs	Dec 4 1991
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
      PARAMETER		(ROUTINE = 'PIXX2AP')
C
      REAL		PI, R2D
      PARAMETER		(PI=3.14159265)
      PARAMETER		(R2D=180.0/PI)
C
      INTEGER		I
      REAL		RA, IA
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 10 I = 1, N
         RA      = REAL (CARR(I))
         IA      = AIMAG(CARR(I))
         AARR(I) = SQRT (RA*RA + IA*IA)
         IF (IA .EQ. 0. .AND. RA .EQ.  0.) THEN
            PARR(I) = 0.
         ELSE
            PARR(I) = ATAN2 (IA, RA) * R2D
            PARR(I) = MOD( (PARR(I)-180.), 360.) +180.
         ENDIF
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
