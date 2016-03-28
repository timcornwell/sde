C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrexts.f	1.3    11/7/90
C
      SUBROUTINE PIXREXTS (A, N1, N2, N3, N4, N5, N6, N7, BLC, TRC, 
     1   EXTSIDE)
C
CD Find largest exterior value
C
C
C	A	REAL	input	Input array
C	N1, N2	INT	input	Number of pixels on each axis of A
C	BLC	INT	input	Start
C	TRC	INT	input	Stop
C	EXTSIDE	REAL	output	Largest value exterior to window
C Audit trail:
C	Previously would not look outside a box but rather an cross-
C	shaped region.
C				T.J.Cornwell	Feb 16 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
C
      REAL		EXTSIDE
      INTEGER		BLC(*), TRC(*), N1, N2, N3, N4, N5, N6, N7
      REAL		A(N1,N2,N3,N4,N5,N6,N7)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXREXTS')
C
      INTEGER		I1, I2, I3, I4, I5, I6, I7
      INTEGER		J1, J2, J3, J4, J5, J6, J7
C=====================================================================
      EXTSIDE = 1.0
C
C Return on input error
C
      IF (ERROR) GO TO 999
C
      EXTSIDE = 0.0
      DO 70 I7 = 1, N7
         DO 60 I6 = 1, N6
            DO 50 I5 = 1, N5
               DO 40 I4 = 1, N4
                  DO 30 I3 = 1, N3
                     DO 20 I2 = 1, N2
                        DO 10 I1 = 1, N1
                           IF(
     1                        (I7.GE.BLC(7)).AND.(I7.LE.TRC(7)).AND.
     1                        (I6.GE.BLC(6)).AND.(I6.LE.TRC(6)).AND.
     1                        (I5.GE.BLC(5)).AND.(I5.LE.TRC(5)).AND.
     1                        (I4.GE.BLC(4)).AND.(I4.LE.TRC(4)).AND.
     1                        (I3.GE.BLC(3)).AND.(I3.LE.TRC(3)).AND.
     1                        (I2.GE.BLC(2)).AND.(I2.LE.TRC(2)).AND.
     1                        (I1.GE.BLC(1)).AND.(I1.LE.TRC(1))
     1                       ) GO TO 10
                              EXTSIDE = MAX(EXTSIDE,
     1                           ABS(A(I1,I2,I3,I4,I5,I6,I7)))
 10                     CONTINUE
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
 60      CONTINUE
 70   CONTINUE
C
      IF(EXTSIDE.EQ.0.0) THEN
         CALL MSGPUT ('Largest exterior sidelobe is zero', 'W')
      END IF
C
 999  CONTINUE
      END
