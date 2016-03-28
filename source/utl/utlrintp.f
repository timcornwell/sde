C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlrintp.f	1.1	 8/10/92
C
      SUBROUTINE UTLRINTP (X, Y, N, UNDEF)
C
C Do linear interpolation to fill in undefined values
C
C	X	REAL	inp	1-D array
C	Y	REAL	inp	1-D array of 
C	N	INT	inp	Number of elements in arrays
C	UNDEF	REAL	inp	Y-value to be replaced by interpolation
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	June 20 1992
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      INTEGER		N
      REAL		X(N), Y(N), UNDEF
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLRINTP')
C
      INTEGER		ISTART, IEND, ILO, IHI, I
C=======================================================================
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 100 ISTART = 1, N
         IF (Y(ISTART).NE.UNDEF) GO TO 101
 100  CONTINUE
C
      CALL ERRREPOR (ERRBDARG, ROUTINE, 'No values in array')
      GO TO 999
 101  CONTINUE
C
      DO 110 IEND = N, 1, -1
         IF (Y(IEND).NE.UNDEF) GO TO 111
 110  CONTINUE
 111  CONTINUE
C
      DO 120 I = 1, ISTART-1
         Y(I) = Y(ISTART)
         IF (X(I).EQ.UNDEF) X(I) = X(ISTART)
 120  CONTINUE
C
      DO 130 I = IEND+1, N
         Y(I) = Y(IEND)
         IF (X(I).EQ.UNDEF) X(I) = X(IEND)
 130  CONTINUE
C
C Main loopback here
C
      ILO = ISTART
 200  CONTINUE
      IF (ILO.EQ.IEND) GO TO 500
C
      IHI = ILO
 210  CONTINUE
      IHI = IHI + 1
      IF (Y(IHI).EQ.UNDEF) GO TO 210
C
      DO 220 I = ILO+1, IHI-1
         IF (X(I).EQ.UNDEF)
     $      X(I) = X(ILO) +
     $         REAL(I - ILO)/(IHI - ILO) * (X(IHI) - X(ILO))
         IF (X(ILO).EQ.X(IHI)) THEN
            IF(X(I).EQ.X(ILO)) THEN
               Y(I) = Y(ILO) +
     $            REAL(I - ILO)/(IHI - ILO) * (Y(IHI) - Y(ILO))
            ELSE
               Y(I) = 1.E20
            END IF
         ELSE
            Y(I) = Y(ILO) +
     $         (Y(IHI)-Y(ILO))/(X(IHI)-X(ILO)) * (X(I)-X(ILO))
         END IF
 220  CONTINUE
C
      ILO = IHI
      GO TO 200
C
 500  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
 

