C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pfftx2d.f	1.1 12/5/92
C
      SUBROUTINE SDEMAIN
C
CD Program to test FFTS in two dimensions
C
C Audit trail:
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PFFTX2D')
C
      INTEGER		NAXIS(SYSMXDIM), IADD, XNAXIS(SYSMXDIM), DIR
      INTEGER		NPAR, I
      CHARACTER*6	STRINT
      DATA		NAXIS	/SYSMXDIM*1/
      CHARACTER*(SYSMXNAM)	IN(20), OUT(20)
C==================================================================
C
      CALL MSGWELCO (
     $   'I test parallel 2D real to complex (and inverse) FFTs')
C
C Now create the data
C
      NAXIS(1) = 32
      NAXIS(2) = 32
C
C Now loop through sizes
C
      NPAR = 5
      DO 1 I = 1, NPAR
         NAXIS(1) = 2 * NAXIS(1)
         NAXIS(2) = 2 * NAXIS(2)
         WRITE (MESSAGE, 1000) I, NAXIS(1), NAXIS(2)
 1000    FORMAT ('Thread ',I3,' is ',I4,' by ',I4)
         CALL MSGPUT (MESSAGE, 'I')
         IN(I) = 'In'//STRINT(NAXIS(1))
         OUT(I) = 'Out'//STRINT(NAXIS(1))
         CALL DATMAKAR (IN(I), 2, NAXIS, 'R', IADD)
         XNAXIS(1) = NAXIS(2)/2 + 1
         XNAXIS(2) = 2 * NAXIS(1)
         CALL DATMAKAR (OUT(I), 2, XNAXIS, 'X', IADD)
         CALL ARRSETCO (IN(I), 0.0, 1.0)
         CALL ARRSETCO (OUT(I), 0.0, 1.0)
 1    CONTINUE
C
C Now do it in parallel
C
      DIR = 1
      CALL MSGPUT ('Forward transform', 'I')
      CALL ARRFFTRH (NPAR, IN, OUT, DIR)
      DIR = -1
      CALL MSGPUT ('Reverse transform', 'I')
      CALL ARRFFTRH (NPAR, IN, OUT, DIR)
C
 999  CONTINUE
      END
