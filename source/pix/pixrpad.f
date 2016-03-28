C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrpad.f	1.3    11/7/90
C
      SUBROUTINE PIXRPAD (A, B, AN1, AN2, AN3, AN4, AN5, AN6, AN7,
     1   BN1, BN2, BN3, BN4, BN5, BN6, BN7, LSHIFT)
C
CD Place A inside B with left margin LSHIFT
C
C
C       A         REAL input   Input array
C       B         REAL output  Output array
C	AN*	INT	input	Number of pixels on axes
C	BN*	INT	input	Number of pixels on axes
C       LSHIFT    INT  input   Left margin along each axis
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER       LSHIFT(*)
      INTEGER	    AN1, AN2, AN3, AN4, AN5, AN6, AN7
      INTEGER	    BN1, BN2, BN3, BN4, BN5, BN6, BN7
      REAL          A(AN1, AN2, AN3, AN4, AN5, AN6, AN7)
      REAL          B(BN1, BN2, BN3, BN4, BN5, BN6, BN7)
C
      CHARACTER*(*) ROUTINE
      PARAMETER     (ROUTINE = 'PIXRPAD')
C
      INTEGER       I1, I2, I3, I4, I5, I6, I7
      INTEGER       J1, J2, J3, J4, J5, J6, J7
C====================================================================
C
C Return on error
C
      IF (ERROR) GO TO 999
C
      DO 70 I7 = 1, AN7
         J7 = I7 + LSHIFT(7)
         DO 60 I6 = 1, AN6
            J6 = I6 + LSHIFT(6)
            DO 50 I5 = 1, AN5
               J5 = I5 + LSHIFT(5)
               DO 40 I4 = 1, AN4
                  J4 = I4 + LSHIFT(4)
                  DO 30 I3 = 1, AN3
                     J3 = I3 + LSHIFT(3)
                     DO 20 I2 = 1, AN2
                        J2 = I2 + LSHIFT(2)
                        DO 10 I1 = 1, AN1
                           J1 = I1 + LSHIFT(1)
                           B(J1,J2,J3,J4,J5,J6,J7) =
     1                        A(I1,I2,I3,I4,I5,I6,I7)
 10                     CONTINUE
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
 60      CONTINUE
 70   CONTINUE
C
 999  CONTINUE
      END
