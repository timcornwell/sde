C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixx2pc.f	1.3	 7/20/92
C
      SUBROUTINE PIXX2PC (N, XARR, AARR, CARR)
C
CD Converts a 2D complex array into AMP and CHI;  Z = AMP * EXP (i 2*CHI)
C
C	N	INT	input	total number of pixels
C	XARR	X	input	Complex Array
C	AARR	R	input	AMP of XARR
C	CARR	R	input	Phase of  XARR
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
      COMPLEX		XARR (*)
      REAL		AARR (*), CARR(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXX2PC')
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
         RA      = REAL (XARR(I))
         IA      = AIMAG(XARR(I))
         AARR(I) = SQRT (RA*RA + IA*IA)
         IF (IA .EQ. 0. .AND. RA .EQ.  0.) THEN
            CARR(I) = 0.
         ELSE
            CARR(I) = ATAN2 (IA, RA) / 2. * R2D
            CARR(I) = MOD((CARR(I) - 90.), 180.) +90.
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
