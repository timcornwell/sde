C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixphase.f	1.3    11/7/90
C
      SUBROUTINE PIXPHASE (A, SHIFT, RPIX, N1, N2, N3, N4, 
     1   N5, N6, N7)
C
CD Phase rotation of an array
C
C
C	A	CMPLX	input	Input array
C	SHIFT	REAL(*)	input	Shift in fraction of the field
C	NAXIS	INT	input	Number of elements
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		N1, N2, N3, N4, N5, N6, N7
      COMPLEX		A(N1, N2, N3, N4, N5, N6, N7)
      REAL		SHIFT(*), RPIX(*)
C
      INTEGER		I1, I2, I3, I4, I5, I6, I7
      REAL		PHASED
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
      COMPLEX		CROT(SYSMXDIM), CROTI(SYSMXDIM), CWEIGHT
C=====================================================================
      IF (ERROR) GO TO 999
      CWEIGHT = CMPLX(1.0, 0.0)
      DO 5 I1 = 1, SYSMXDIM
         PHASED = 2*PI*SHIFT(I1)*(1.0-RPIX(I1))
         CROTI(I1) = CMPLX (COS(PHASED), -SIN(PHASED))
         PHASED = 2*PI*SHIFT(I1)
         CROT(I1) = CMPLX (COS(PHASED), -SIN(PHASED))
  5   CONTINUE
      DO 70 I7 = 1, N7
         DO 60 I6 = 1, N6
            DO 50 I5 = 1, N5
               DO 40 I4 = 1, N4
                  DO 30 I3 = 1, N3
                     DO 20 I2 = 1, N2
C
C Avoid excessive rounding error by re-evaulating weights often
C
                        CWEIGHT = CROTI(1)*CROTI(2)*CROT(2)**(I2-1)*
     1                                     CROTI(3)*CROT(3)**(I3-1)*
     1                                     CROTI(4)*CROT(4)**(I4-1)*
     1                                     CROTI(5)*CROT(5)**(I5-1)*
     1                                     CROTI(6)*CROT(6)**(I6-1)*
     1                                     CROTI(7)*CROT(7)**(I7-1)
                        DO 10 I1 = 1, N1
                           A(I1,I2,I3,I4,I5,I6,I7) = CWEIGHT *
     1                        A(I1,I2,I3,I4,I5,I6,I7)
                           CWEIGHT = CWEIGHT * CROT(1)
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
