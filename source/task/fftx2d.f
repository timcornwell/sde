C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftx2d.f	1.6    7/14/97
C
      SUBROUTINE SDEMAIN
C
CD Program to test FFTS in two dimensions
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Remove initialization from timing
C				T.J.Cornwell	Oct 29 1990
C	Initialize contents of arrays to avoid NaNs on sparcs
C				T.J.Cornwell	June 17 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTX2D')
C
      INTEGER		NAXIS(SYSMXDIM), IADD, XNAXIS(SYSMXDIM), DIR
      REAL		SYSECPUT, STARTTIM, ENDTIM, XDUMMY
      CHARACTER*(SYSMXNAM)	IN, OUT
      CHARACTER*6	STRINT
      DATA		NAXIS	/SYSMXDIM*1/
C==================================================================
C
      CALL MSGWELCO ('I time 2D real to complex (and inverse) FFTs')
C
C Now create the data
C
      NAXIS(1) = 32
      NAXIS(2) = 32
C
C Now loop through sizes
C
 1    CONTINUE
C
      NAXIS(1) = 2 * NAXIS(1)
      NAXIS(2) = 2 * NAXIS(2)
C
C Make 2D arrays
C
      IN = 'In'//STRINT(NAXIS(1))
      OUT = 'Out'//STRINT(NAXIS(1))
      CALL DATMAKAR (IN, 2, NAXIS, 'R', IADD)
      XNAXIS(1) = NAXIS(2)/2 + 1
      XNAXIS(2) = 2 * NAXIS(1)
      CALL DATMAKAR (OUT, 2, XNAXIS, 'X', IADD)
      CALL ARRSETCO (IN, 0.0, 1.0)
      CALL ARRSETCO (OUT, 0.0, 1.0)
C
C Do one FFT to initialize for this size
C
      DIR = 0
      CALL ARRFFTR (IN, OUT, DIR)
C
C Now time it
C
      STARTTIM = SYSECPUT(XDUMMY)
      DIR = 1
      CALL ARRFFTR (IN, OUT, DIR)
      DIR = -1
      CALL ARRFFTR (IN, OUT, DIR)
      ENDTIM = SYSECPUT(XDUMMY)
      IF(ERROR) GO TO 999
      WRITE (MESSAGE, 1000) 'Time per ',NAXIS(1),' by ', NAXIS(2),
     $   ' FFT = ', (ENDTIM-STARTTIM)/2.0, ' seconds'
 1000 FORMAT (A,I4,A,I4,A,F10.3,A)
      CALL MSGPUT (MESSAGE, 'I')
      IF(NAXIS(1).LT.1024) GO TO 1
C
 999  CONTINUE
      END
