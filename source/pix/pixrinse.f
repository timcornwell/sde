C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrinse.f	1.3    11/7/90
C
      SUBROUTINE PIXRINSE (A, B, AN1, AN2, AN3, AN4, AN5, AN6, AN7,
     1   BN1, BN2, BN3, BN4, BN5, BN6, BN7, BLC, TRC, STEP)
C
CD Add array A into array B
C
C   BLC, TRC, STEP)
C
C	A	REAL	input	Input array
C	B	REAL	output	Output array
C	ANAXIS	INT	input	Number of pixels on each axis of A
C	BNAXIS	INT	input	Number of pixels on each axis of B
C	BLC	INT	input	Start
C	TRC	INT	input	Stop
C	STEP	INT	input	Increment
C Audit trail:
C	Reversed indexing to speed it up a bit
C				T.J.Cornwell	Feb 24 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		BLC(*), TRC(*), STEP(*)
      INTEGER		AN1, AN2, AN3, AN4, AN5, AN6, AN7
      INTEGER		BN1, BN2, BN3, BN4, BN5, BN6, BN7
      REAL		A(AN1,AN2,AN3,AN4,AN5, AN6, AN7)
      REAL		B(BN1,BN2,BN3,BN4,BN5, BN6, BN7)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRINSE')
C
      INTEGER		I1, I2, I3, I4, I5, I6, I7
      INTEGER		J1, J2, J3, J4, J5, J6, J7
C=====================================================================
C
C Return on input error
C
      IF (ERROR) GO TO 999
C
      DO 70 J7 = 1, (TRC(7)-BLC(7)+1)/STEP(7)
         I7 = (J7 - 1) * STEP(7) + BLC(7)
         DO 60 J6 = 1, (TRC(6)-BLC(6)+1)/STEP(6)
            I6 = (J6 - 1) * STEP(6) + BLC(6)
            DO 50 J5 = 1, (TRC(5)-BLC(5)+1)/STEP(5)
               I5 = (J5 - 1) * STEP(5) + BLC(5)
               DO 40 J4 = 1, (TRC(4)-BLC(4)+1)/STEP(4)
                  I4 = (J4 - 1) * STEP(4) + BLC(4)
                  DO 30 J3 = 1, (TRC(3)-BLC(3)+1)/STEP(3)
                     I3 = (J3 - 1) * STEP(3) + BLC(3)
                     DO 20 J2 = 1, (TRC(2)-BLC(2)+1)/STEP(2)
                        I2 = (J2 - 1) * STEP(2) + BLC(2)
                        DO 10 J1 = 1, (TRC(1)-BLC(1)+1)/STEP(1)
                           I1 = (J1 - 1) * STEP(1) + BLC(1)
                           B(I1,I2,I3,I4,I5,I6,I7) = 
     1                        B(I1,I2,I3,I4,I5,I6,I7) +
     1                        A(J1,J2,J3,J4,J5,J6,J7)
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
