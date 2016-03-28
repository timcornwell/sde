C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdprin.f	1.1    7/27/92
C
      SUBROUTINE PIXDPRIN (A, BLC, TRC, N1, N2, N3, N4, N5, N6, N7)
C
CD Print an array
C
C
C	A	DBLE	input	Input array
C	BLC	INT	input	Start
C	TRC	INT	input	Stop
C	NAXIS	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		BLC(*), TRC(*), N1, N2, N3, N4, N5, N6, N7
      DOUBLE PRECISION	A(N1, N2, N3, N4, N5, N6, N7)
C
C
      INTEGER		I1, I2, I3, I4, I5, I6, I7
C=====================================================================
      IF (ERROR) GO TO 999
      DO 70 I7 = BLC(7), TRC(7)
         DO 60 I6 = BLC(6), TRC(6)
            DO 50 I5 = BLC(5), TRC(5)
               DO 40 I4 = BLC(4), TRC(4)
                  DO 30 I3 = BLC(3), TRC(3)
                     DO 20 I2 = BLC(2), TRC(2)
                        DO 10 I1 = BLC(1), TRC(1)
                           WRITE (MESSAGE, 2000) I1, I2, I3, I4, I5,
     1                        I6, I7, A(I1,I2,I3,I4,I5,I6,I7)
 2000                      FORMAT (7(I4,1X),1PE15.7)
                           CALL MSGPUT (MESSAGE, 'I')
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
