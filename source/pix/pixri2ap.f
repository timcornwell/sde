C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixri2ap.f	1.4	 7/20/92
C
      SUBROUTINE PIXRI2AP (A, B, C, NX, NY)
C
C Convert the complex real, imag. data in A to the real
C amplitude, phase in B, C
C Pixel level routine. 2-D complex to 2-D real only.
C
C
C	A	CMPX(*)	input	Image
C	B	REAL(*)	input	Amplitudes
C	C	REAL(*)	input	Phases
C	NX	INT	input	Number of points in x dir
C	NY	INT	input	Number of points in y dir
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX,NY
      COMPLEX		A(NX,*)
      REAL		B(NX,*), C(NX,*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRI2AP')
C
      INTEGER		IX,IY
      REAL		REPART, IMPART, PIBY2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      PIBY2 = 1.570796
      DO 20 IY = 1, NY
         DO 10 IX = 1, NX        
            REPART = REAL(A(IX,IY))
            IMPART = AIMAG(A(IX,IY))
            B(IX,IY) = ABS(A(IX,IY))
            IF (REPART.NE.0) THEN
               C(IX,IY) = ATAN2(IMPART, REPART)
             ELSE
               C(IX,IY) = SIGN(IMPART, PIBY2)
            END IF
  10     CONTINUE
  20  CONTINUE
C
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
