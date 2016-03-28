
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C  @(#)fftx.f	1.4 6/1/91
C
      SUBROUTINE SDEMAIN
C
CD Program to test FFTS
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C Audit trail:
C	Stop at 4096 until we fix the 8192 case
C				T.J. Cornwell	June 1 1991
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTX')
C
      COMPLEX 		X(8192), WORK(8192),T(16)
      INTEGER		I, N, NLOOP, 
     1			ILOOP
      REAL		STARTTIM, ENDTIME, SYSECPUT, XDUMMY
      REAL              EPSILON
C==================================================================
C
      CALL MSGWELCO ('I time FFTs')
C
C Now create the data
C
      DO 2 I = 1, 16
         T(I) = 0.0
 2    CONTINUE
      T(16) = 1.0
      EPSILON = T(16) * 1.0E-5
      N = 128
 3    N = N * 2
      CALL FFTX (X, WORK, N, +1)
      DO 10 I=1,N
         X(I) = 0.0
  10  CONTINUE
      X(16) = 1.0
      NLOOP = 1000
      NLOOP = 200
C
C First test to see it it works
C
      CALL FFTX (X, WORK, N, +1)
         IF (ABS(T(16)-X(1)).GT.EPSILON) THEN
            CALL MSGPUT ('Forward:', 'I')
            WRITE (MESSAGE, 1000) I, X(I)
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
 1000    FORMAT (1X, 'X(',I2,')',' = (',F10.5,',',F10.5,')')
      CALL FFTX (X, WORK, N, -1)
C
C Now time it
C
      STARTTIM = SYSECPUT(XDUMMY)
      DO 50 ILOOP = 1, NLOOP
         CALL FFTX (X, WORK, N, +1)
         CALL FFTX (X, WORK, N, -1)
  50  CONTINUE
      ENDTIME = SYSECPUT(XDUMMY)
      WRITE (MESSAGE, 1010) 'Time per ',N,' complex FFT = ',
     1   1000*(ENDTIME-STARTTIM)/(2*NLOOP), ' milliseconds'
 1010 FORMAT (A,I5,A,F10.3,A)
      CALL MSGPUT (MESSAGE, 'I')
      DO 25 I = 1, 16
         IF (ABS(T(I)-X(I)).GT.EPSILON) THEN
            WRITE (MESSAGE, 1000) I, X(I)
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
 25   CONTINUE
 999  CONTINUE
      IF (N.LT.4096) GOTO 3
      END
