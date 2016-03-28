C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxsubs.f	1.2    2/1/93
C
      SUBROUTINE PIXXSUBS (A, B, AN1, AN2, AN3, AN4, AN5, AN6,
     1   AN7, BN1, BN2, BN3, BN4, BN5, BN6, BN7, BLC, TRC, 
     1   STEP, SUM)
C
CD Subsection a COMPLEX array
C
C	A	COMPLEX	input	Input array
C	B	COMPLEX	output	Output array
C	ANn	INT	input	Number of pixels on n'th axis of A
C	BNn	INT	input	Number of pixels on n'th axis of B
C	BLC	INT	input	Start
C	TRC	INT	input	Stop
C	STEP	INT	input	Increment
C	SUM	INT	input   Sum
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Dec 3 1990
C       Added Summing
C				 R.G Marson 5 Mar 1992
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM)
      INTEGER           STEP(SYSMXDIM), SUM(SYSMXDIM)
      INTEGER		AN1, AN2, AN3, AN4, AN5, AN6, AN7
      INTEGER		BN1, BN2, BN3, BN4, BN5, BN6, BN7
      COMPLEX		A(AN1, AN2, AN3, AN4, AN5, AN6, AN7)
      COMPLEX		B(BN1, BN2, BN3, BN4, BN5, BN6, BN7)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXXSUBS')
C
      INTEGER		I1, I2, I3, I4, I5, I6, I7
      INTEGER		J1, J2, J3, J4, J5, J6, J7
      INTEGER		K1, K2, K3, K4, K5, K6, K7
      INTEGER		TBLC(7)
C=====================================================================
C
C Return on input error
C
      IF (ERROR) GO TO 999
C
      DO 666 K7 = 1, SYSMXDIM
         TBLC(K7) = BLC(K7)
 666  CONTINUE
C
      DO 170 K7 = 1, SUM(7)
       DO 160 K6 = 1, SUM(6)
        DO 150 K5 = 1, SUM(5)
         DO 140 K4 = 1, SUM(4)
          DO 130 K3 = 1, SUM(3)
           DO 120 K2 = 1, SUM(2)
            DO 110 K1 = 1, SUM(1)
             DO 70 I7 = TBLC(7), TRC(7), STEP(7)
              J7 = (I7 - TBLC(7))/STEP(7) + 1
              DO 60 I6 = TBLC(6), TRC(6), STEP(6)
               J6 = (I6 - TBLC(6))/STEP(6) + 1
               DO 50 I5 = TBLC(5), TRC(5), STEP(5)
                J5 = (I5 - TBLC(5))/STEP(5) + 1
                DO 40 I4 = TBLC(4), TRC(4), STEP(4)
                 J4 = (I4 - TBLC(4))/STEP(4) + 1
                 DO 30 I3 = TBLC(3), TRC(3), STEP(3)
                  J3 = (I3 - TBLC(3))/STEP(3) + 1
                  DO 20 I2 = TBLC(2), TRC(2), STEP(2)
                   J2 = (I2 - TBLC(2))/STEP(2) + 1
                   DO 10 I1 = TBLC(1), TRC(1), STEP(1)
                    J1 = (I1 - TBLC(1))/STEP(1) + 1
                    B(J1,J2,J3,J4,J5,J6,J7) =
     1                   A(I1,I2,I3,I4,I5,I6,I7) + 
     $                   B(J1,J2,J3,J4,J5,J6,J7) 
 10                CONTINUE
 20               CONTINUE
 30              CONTINUE
 40             CONTINUE
 50            CONTINUE
 60           CONTINUE
 70          CONTINUE
             TBLC(1) = BLC(1) + K1
 110        CONTINUE
            TBLC(2) = BLC(2) + K2
 120       CONTINUE
           TBLC(3) = BLC(3) + K3
 130      CONTINUE
          TBLC(4) = BLC(4) + K4
 140     CONTINUE
         TBLC(5) = BLC(5) + K5
 150    CONTINUE
        TBLC(6) = BLC(6) + K6
 160   CONTINUE
       BLC(7) = BLC(7) + K7
 170  CONTINUE
C
 999  CONTINUE
      END
