C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdif.f	1.3    11/7/90
C
       SUBROUTINE GRDIF (IFCODE, IF, IFSUPP, IFOSAMP)
C
CD Calculate interpolation functions
C
C
C	IFCODE	CH*(*)	input	Code of IF e.g. 'SINC'
C	IF	REAL(*)	output	Convolution function
C	IFSUPP	INT	output	Support of IF
C	IFOSAMP	INT	output	Over sampling factor for IF
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	IFCODE
      INTEGER		IFSUPP, IFOSAMP
      REAL		IF(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GRDIF')
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      INTEGER		I
      REAL		SINC, NU, X
C======================================================================
      SINC(X) = SIN(PI*X) / (PI*X)
C
      IF (ERROR) GO TO 999
C
      IFOSAMP = 128
      IF (IFCODE(1:3).EQ.'BOX') THEN
         IFSUPP = 1
         DO 10 I = 1, IFOSAMP/2+1
            IF(I) = 1.0
  10     CONTINUE
         DO 15 I = IFOSAMP/2+2, 2*IFOSAMP+1
            IF(I) = 0.0
  15     CONTINUE
      ELSEIF (IFCODE(1:4).EQ.'SINC') THEN
         IFSUPP = 5
         IF(1) = 1
         DO 20 I = 2, IFSUPP*IFOSAMP + 1
            NU = FLOAT(I-1)/FLOAT(IFOSAMP)
            IF(I) = SINC(NU)
  20     CONTINUE
         DO 25 I = IFSUPP*IFOSAMP+2, (IFSUPP+1)*IFOSAMP+1
            IF(I) = 0.0
  25     CONTINUE
      ELSE
         WRITE (MESSAGE, 1000) IFCODE
 1000    FORMAT ('Cannot make interpolation function ',A)
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
