C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlrmed.f	1.1    11/7/94
C
      SUBROUTINE UTLRMED (ARR, N, MEDIAN)
C
C A simple median finder
C
C	ARR	REAL	input	1-D array
C	N	INT	input	size of ARR
C	MEDIAN	REAL	output	median of array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	11 June 1993
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      INTEGER		N
      REAL		ARR(N), MEDIAN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLRMED')
C
      INTEGER		I
C
      INTEGER		TMPSIZ, TMPADD
      SAVE		TMPSIZ, TMPADD
      DATA		TMPSIZ /0/
C=======================================================================
      IF (ERROR) GO TO 999
C
      IF (N.GT.TMPSIZ) THEN
         CALL DATDELET ('UTLRMED-TMP')
         CALL DATMAKAR ('UTLRMED-TMP', 1, NINT(1.3*N), 'I', TMPADD)
      END IF
C
      CALL UTLRHSRT (ARR, N, 1, MEMI(TMPADD))
C
      I = N / 2
      IF (2*I.EQ.N) THEN
         MEDIAN = (ARR(MEMI(TMPADD+I-1)) + ARR(MEMI(TMPADD+I))) / 2.0
      ELSE
         MEDIAN = ARR(MEMI(TMPADD+I))
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
 

