C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdccf.f	1.5    3/3/91
C
       SUBROUTINE GRDCCF (CCFCODE, CCF, CCFLEN)
C
CD Calculate convolution correction functions.
C
C
C	CCFCODE	CH*(*)	input	Code of CCF e.g. 'SF', 'BOX', 'SZE'
C	CCF	REAL(*)	output	Convolution correction function
C	CCFLEN	INT	output	Length of CCF
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed SF stuff to double precision
C				T.J.Cornwell	March 3 1991
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	CCFCODE
      INTEGER		CCFLEN
      REAL		CCF(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GRDCCF')
C
      INTEGER		I
      DOUBLE PRECISION	GRDSF, NU
      REAL		GRDSZE, SINC, X, PI
      DATA		PI	/3.141592554/
C======================================================================
      SINC(X) = SIN(X)/(X)
C
      IF (ERROR) GO TO 999
C
      IF (CCFCODE(1:3).EQ.'BOX') THEN
         CCF(1) = 1.0
         DO 10 I = 2, CCFLEN
            NU = DBLE(I-1)/DBLE(CCFLEN-1)
            CCF(I) = SINC(0.5*PI*SNGL(NU))
 10      CONTINUE
      ELSEIF (CCFCODE(1:2).EQ.'SF') THEN
         DO 20 I = 1, CCFLEN
            NU = DBLE(I-1)/DBLE(CCFLEN-1)
            CCF(I) = SNGL(GRDSF(NU))
  20     CONTINUE
      ELSEIF (CCFCODE(1:3).EQ.'SZE') THEN
         DO 30 I = 1, CCFLEN
            NU = DBLE(I-1)/DBLE(CCFLEN-1)
            CCF(I) = GRDSZE(SNGL(NU))
  30     CONTINUE
      ELSE
         WRITE (MESSAGE, 1000) CCFCODE
 1000    FORMAT ('Cannot make convolution correction function ',A)
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
