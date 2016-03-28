C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdcf.f	1.4    3/3/91
C
       SUBROUTINE GRDCF (CFCODE, CF, CFSUPP, CFOSAMP)
C
CD Calculate convolution functions: Normalize to unit volume
C
C
C	CFCODE	CH*(*)	input	Code of CF e.g. 'SF', 'BOX', 'SZE'
C	CF	REAL(*)	output	Convolution function
C	CFSUPP	INT	output	Support of CF
C	CFOSAMP	INT	output	Over sampling factor for CF
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed to fully symmetric version
C				T.J.Cornwell	Nov 19 1989
C	Changed SF stuff to double precision
C				T.J.Cornwell	March 3 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	CFCODE
      INTEGER		CFSUPP, CFOSAMP
      REAL		CF(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GRDCF')
C
      INTEGER		I, CFOFFSET
      DOUBLE PRECISION	GRDSF, NU
      REAL		GRDSZE, SUM
C======================================================================
      IF (ERROR) GO TO 999
C
      CFOSAMP = 128
      IF (CFCODE(1:3).EQ.'BOX') THEN
         CFSUPP = 1
         CFOFFSET = (CFSUPP+1)*CFOSAMP + 1
         DO 10 I = 0, CFOSAMP/2
            CF(CFOFFSET+I) = 1.0
            CF(CFOFFSET-I) = 1.0
  10     CONTINUE
         DO 15 I = CFOSAMP/2+1, 2*CFOSAMP
            CF(CFOFFSET+I) = 0.0
            CF(CFOFFSET-I) = 0.0
  15     CONTINUE
      ELSEIF (CFCODE(1:2).EQ.'SF') THEN
         CFSUPP = 3
         CFOFFSET = (CFSUPP+1)*CFOSAMP + 1
         DO 20 I = 0, CFSUPP*CFOSAMP
            NU = DBLE(I)/DBLE(CFSUPP*CFOSAMP)
            CF(CFOFFSET+I) = SNGL((1-NU**2)*GRDSF(NU))
            CF(CFOFFSET-I) = CF(CFOFFSET+I)
  20     CONTINUE
         DO 25 I = CFSUPP*CFOSAMP+1, (CFSUPP+1)*CFOSAMP
            CF(CFOFFSET+I) = 0.0
            CF(CFOFFSET-I) = 0.0
  25     CONTINUE
      ELSEIF (CFCODE(1:3).EQ.'SZE') THEN
         CFSUPP = 3
         CFOFFSET = (CFSUPP+1)*CFOSAMP + 1
         DO 30 I = 0, CFSUPP*CFOSAMP
            NU = DBLE(I)/DBLE(CFOSAMP)
            CF(CFOFFSET+I) = GRDSZE(SNGL(NU))
            CF(CFOFFSET-I) = CF(CFOFFSET+I)
  30     CONTINUE
         DO 35 I = CFSUPP*CFOSAMP+1, (CFSUPP+1)*CFOSAMP
            CF(CFOFFSET+I) = 0.0
            CF(CFOFFSET-I) = 0.0
  35     CONTINUE
      ELSE
         WRITE (MESSAGE, 1000) CFCODE
 1000    FORMAT ('Cannot make convolution function ',A)
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
      SUM = 0.0
      DO 40 I = -(CFSUPP+1)*CFOSAMP, (CFSUPP+1)*CFOSAMP
         SUM = SUM + CF(CFOFFSET+I)
  40  CONTINUE
      DO 50 I = -(CFSUPP+1)*CFOSAMP, (CFSUPP+1)*CFOSAMP
         CF(CFOFFSET+I) = CF(CFOFFSET+I) / SUM
  50  CONTINUE
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
